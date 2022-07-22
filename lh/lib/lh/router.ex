defmodule LH.Router do
  require Logger
  use Plug.Router

  plug(Plug.Logger)
  plug(:match)
  plug(Plug.Parsers, parsers: [:json], json_decoder: Jason)
  plug(:dispatch)

  get "/" do
    msg = LH.Lightning.hfr()

    conn
    |> send_resp(200, msg)
    |> halt()
  end

  get "/ping" do
    conn
    |> send_resp(200, "")
    |> halt()
  end

  get "/shutdown" do
    conn
    |> send_resp(200, "Shutting down...")
    |> halt()

    shutdown()
  end

  post "/git-info" do
    {status, body} =
      case conn.body_params do
        %{"path" => path} ->
          {200, git_info(path)}

        _ ->
          {422, bad_request_body()}
      end

    conn
    |> put_resp_header("content-type", "application/json;charset=utf-8")
    |> send_resp(status, body)
    |> halt()
  end

  post "/path-shorten" do
    {status, body} =
      case conn.body_params do
        %{"name" => path, "aliases_raw" => aliases_raw, "substitutions" => subs} ->
          {200, path_shorten(path, aliases_raw, subs)}

        _ ->
          {422, bad_request_body()}
      end

    conn
    |> put_resp_header("content-type", "application/json;charset=utf-8")
    |> send_resp(status, body)
    |> halt()
  end

  post "/paths-sort" do
    {status, body} =
      case conn.body_params do
        %{"paths" => paths, "priorities_raw" => priorities_raw, "home" => home} ->
          {200, paths_sort(paths, priorities_raw, home)}

        _ ->
          {422, bad_request_body()}
      end

    conn
    |> put_resp_header("content-type", "application/json;charset=utf-8")
    |> send_resp(status, body)
    |> halt()
  end

  # Strings in Elixir are represented as binaries, so we use is_binary/1 instead
  # of is_string/1.
  defp git_info(path)
       when is_binary(path) do
    # Shell out to git to get additional information for those parts that are
    # faster using the vanilla git binary instead of using the Rust bindings for
    # libgit2. We use Task.async to basically "background" these processes while
    # we do other work.
    task_get_diff_stats = Task.async(fn -> LH.Git.diff(path) end)
    task_get_diff_cached_stats = Task.async(fn -> LH.Git.diff_cached(path) end)

    json = LH.Lightning.git_info(path)
    git_info = Poison.decode!(json, as: %LH.Git{})

    # For this task, we use the root of the repo (calculated by
    # LH.Lightning.git_info) because it is sensitive to the PWD in which the
    # command runs.
    task_get_untracked_files = Task.async(fn -> LH.Git.untracked_files(git_info.root) end)

    diff_stats = Task.await(task_get_diff_stats)
    diff_cached_stats = Task.await(task_get_diff_cached_stats)
    untracked_files = Task.await(task_get_untracked_files)

    unstaged = %{
      unstaged_files: Map.get(diff_stats, :files, 0),
      unstaged_insertions: Map.get(diff_stats, :insertions, 0),
      unstaged_deletions: Map.get(diff_stats, :deletions, 0)
    }

    staged = %{
      staged_files: Map.get(diff_cached_stats, :files, 0),
      staged_insertions: Map.get(diff_cached_stats, :insertions, 0),
      staged_deletions: Map.get(diff_cached_stats, :deletions, 0)
    }

    git_info = Map.merge(git_info, unstaged)
    git_info = Map.merge(git_info, staged)
    git_info = Map.replace!(git_info, :untracked_files, untracked_files)

    Logger.info("/git-info: #{inspect(path)} => #{inspect(git_info)}")

    Poison.encode!(git_info)
  end

  defp git_info(path) do
    if not is_binary(path) do
      Logger.error("/git-info: path is not a string: #{path}")
    end

    Jason.encode!(%{
      error: "bad arguments"
    })
  end

  # Strings in Elixir are represented as binaries, so we use is_binary/1 instead
  # of is_string/1.
  defp path_shorten(path, aliases_raw, subs)
       when is_binary(path) and is_binary(aliases_raw) and is_map(subs) do
    {status, msg} = Cachex.get(:path_shorten_cache, {path, aliases_raw, subs})

    {msg_final, cached_status} =
      if status == :error || msg == nil do
        msg =
          LH.Lightning.path_shorten(
            path,
            aliases_raw,
            subs
          )

        Cachex.put(:path_shorten_cache, {path, aliases_raw, subs}, msg)
        {msg, :MIS}
      else
        {msg, :HIT}
      end

    Logger.info("/path-shorten: (#{cached_status}) #{inspect(path)} => #{inspect(msg_final)}")

    Jason.encode!(%{path_shortened: msg_final})
  end

  defp path_shorten(path, aliases_raw, subs) do
    if not is_binary(path) do
      Logger.error("/path-shorten: path is not a string: #{path}")
    end

    if not is_binary(aliases_raw) do
      Logger.error("/path-shorten: aliases_raw is not a string: #{aliases_raw}")
    end

    if not is_map(subs) do
      Logger.error("/path-shorten: subs is not a map: #{subs}")
    end

    Jason.encode!(%{
      error: "bad arguments"
    })
  end

  defp paths_sort(paths, priorities_raw, home)
       when is_binary(paths) and is_binary(priorities_raw) and is_binary(home) do
    {status, msg} = Cachex.get(:paths_sort_cache, {paths, priorities_raw, home})

    {msg_final, cached_status} =
      if status == :error || msg == nil do
        msg = LH.Lightning.paths_sort(paths, priorities_raw, home)

        Cachex.put(:paths_sort_cache, {paths, priorities_raw, home}, msg)
        {msg, :MIS}
      else
        {msg, :HIT}
      end

    Logger.info("/paths-sort: (#{cached_status}) #{inspect(paths)} => #{inspect(msg_final)}")

    Jason.encode!(%{paths_sorted: msg_final})
  end

  defp paths_sort(paths, priorities_raw, home) do
    if not is_binary(paths) do
      Logger.error("/path-shorten: paths is not a string: #{paths}")
    end

    if not is_binary(priorities_raw) do
      Logger.error("/path-shorten: priorities_raw is not a string: #{priorities_raw}")
    end

    if not is_binary(home) do
      Logger.error("/path-shorten: home is not a string: #{home}")
    end

    Jason.encode!(%{
      error: "bad arguments"
    })
  end

  defp bad_request_body do
    Jason.encode!(%{
      error: "bad_request_body"
    })
  end

  defp shutdown do
    System.stop(0)
  end

  # A catchall route to match anything else that does not line up to the
  # expected endpoints above.
  match _ do
    conn
    |> send_resp(404, "Oops!")
    |> halt()
  end
end
