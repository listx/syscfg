defmodule LH.GitWatcher do
  @moduledoc """
  Watches a single directory (technically this can be multiple directories, but
  in practice we should just watch a single one...). Handles filesystem events
  to the watched directory.
  """

  # If a watcher process dies, don't restart it, because it will be restarted on
  # the next use. We still want to use a supervisor for this because this
  # isolates failures of one watcher to itself.
  use GenServer, restart: :temporary
  require Logger

  # 1 second. This is how quickly we can call LH.GitRepo.repo_stats/1 which is
  # expensive. If we detect 0 filesystem changes during the 1-second interval,
  # we just use the previously-cached version.
  @tick_interval 1000

  def start_link(repo_path) when is_binary(repo_path) do
    if String.length(repo_path) > 0 do
      case LH.GitRepo.get_repo_root(repo_path) do
        {:ok, repo_id} ->
          GenServer.start_link(__MODULE__, %{repo_id: repo_id}, name: via_tuple(repo_path))

        {:error, :no_repo_id} ->
          Logger.info("unable to find root git repo for path #{repo_path}")

        {:error, msg} ->
          Logger.warn(msg)
      end
    else
      Logger.info("unable to start watcher for empty git repo path")
    end
  end

  defp via_tuple(repo_id) do
    LH.ProcessRegistry.via_tuple({__MODULE__, repo_id})
  end

  @impl true
  def init(%{repo_id: repo_id} = args) do
    if String.length(repo_id) == 0 do
      raise("cannot start git watcher for 0-length repo_id")
    end

    IO.puts("Starting watcher for #{inspect(args)}")

    # Start the low-level FileSystem watcher lib for the given args.
    #
    # NOTE: On Mac, if you get the error "Can't find executable `mac_listener`",
    # you must run `mix deps.compile file_system` to compile the "mac_listener"
    # application.
    {:ok, watcher_pid} = FileSystem.start_link(dirs: [repo_id])
    FileSystem.subscribe(watcher_pid)

    repo_stats = LH.GitRepo.repo_stats(repo_id)

    # Start up the tick process to begin re-evaluating the full Git information
    # at a regular interval (but only if the cache entry is missing).
    tick()

    # This is the initial state of the watcher. We save the pid of the low-level
    # watcher and also the main repo_id which this watcher is responsible for.
    # We need to save this repo_id so that we can use it to reinstate the
    # repo_stats state if it is missing.
    {:ok, %{watcher_pid: watcher_pid, repo_id: repo_id, repo_stats: repo_stats, stale: false}}
  end

  @impl true
  def handle_info(
        {:file_event, watcher_pid, {path, events}},
        %{
          watcher_pid: watcher_pid,
          repo_id: _repo_id,
          repo_stats: _repo_stats,
          stale: _stale
        } = state
      ) do
    maybe_leaf_path =
      cond do
        # For index files (".git/**/index.lock"), only ignore events that are
        # not :modified ones. When we do get a :modified event, this can happen
        # if HEAD moves, which is important when we (for example) modify the
        # HEAD of submodules.
        LH.Lightning.is_git_index_file(path) ->
          if Enum.member?(events, :modified) do
            {:ok, path}
          else
            {:error, :ignored}
          end

        true ->
          Logger.info("PATH #{path} was #{inspect(events)}")
          {:ok, path}
      end

    case maybe_leaf_path do
      {:ok, leaf_path} ->
        # Invalidate the cache entry for all current and parent GitWatchers.
        mark_all_stale(leaf_path)

        {:noreply, state}

      {:error, :ignored} ->
        Logger.debug(
          "IGNORING GIT INDEX PATH #{path} PID:#{inspect(self())} events:#{inspect(events)}"
        )

        {:noreply, state}
    end
  end

  def handle_info(
        {:file_event, watcher_pid, :stop},
        %{
          watcher_pid: watcher_pid,
          repo_id: repo_id,
          repo_stats: _repo_stats,
          stale: _stale
        } = state
      ) do
    Logger.info("repo #{repo_id}: FileSystem monitor stopped")

    {:noreply, state}
  end

  # Process tick.
  def handle_info(
        :tick,
        %{
          watcher_pid: _watcher_pid,
          repo_id: repo_id,
          repo_stats: repo_stats,
          stale: stale
        } = state
      ) do
    repo_stats =
      if stale do
        Logger.info("RE-EVALUATING git repo stats for #{repo_id}")
        LH.GitRepo.repo_stats(repo_id)
      else
        repo_stats
      end

    # Continue ticking for the future.
    tick()
    {:noreply, %{state | repo_stats: repo_stats, stale: false}}
  end

  @impl true
  def handle_call(:get_repo_stats, _, %{repo_stats: repo_stats} = state) do
    {
      :reply,
      # Response to the caller.
      repo_stats,
      # New state of this GenServer.
      state
    }
  end

  @impl true
  def handle_cast(:mark_stale, %{stale: _} = state) do
    {:noreply, %{state | stale: true}}
  end

  # Send a "tick" message to our GenServer in 2 seconds. See https://stackoverflow.com/a/32097971/437583.
  defp tick() do
    # Send after 2 seconds. We could alternatively use :timer.send_interval
    # (Erlang function) in init/1 and avoid calling this function manually in
    # handle_info/2, but then that would send the tick at a constant rate,
    # regardless of how long it takes to process the tick. This runs the risk of
    # growing the message queue at a faster rate than it can be processed
    # (unbounded growth).
    Process.send_after(self(), :tick, @tick_interval)
  end

  # Client interface.
  #
  # Clients just need to ask us "hey, get the repo state of this repo_id". We
  # then ask the registry to find the process that was started for this repo_id,
  # and then ask that process to return the repo state. Notice how we don't have
  # to know about the PID of the GitWatcher process because the via_tuple
  # retrieves it from the process registry.
  def get_repo_stats(repo_id) do
    # Start the Git watcher for this path. This is idempotent and will not spawn
    # a new watcher if one already exists for this path.
    _pid = LH.Git.start_watcher(repo_id)
    GenServer.call(via_tuple(repo_id), :get_repo_stats)
  end

  # Mark the given path as stale, but also all repos in parent dirs as well.
  def mark_all_stale(path) do
    get_all_subpaths(path)
    |> Enum.map(&mark_stale(&1))
  end

  # Given "/a/b/c", return ["/", "/a", "/a/b", "/a/b/c"]
  def get_all_subpaths(path) do
    parts = Path.split(path)
    parts_len = length(parts)

    1..parts_len
    |> Enum.map(&(Enum.take(parts, &1) |> Path.join()))
  end

  def normalize_path(path) do
    path
    |> Path.split()
    |> Path.join()
  end

  # Mark the given repo as stale. This forces the associated GitWatcher to
  # reevaluate everything (discard the cached version).
  def mark_stale(repo_id) do
    GenServer.cast(via_tuple(repo_id), :mark_stale)
  end
end
