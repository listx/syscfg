defmodule LH.GitRepo do
  @moduledoc """
  Data structure for holding status information about a Git repo.

  Uses a combination of LH.Lightning.repo_stats() and also Git binary CLI calls
  to get this information. The latter is used in cases where the equivalent
  libgit2 Rust binding is too slow.
  """
  @derive [Poison.Encoder]
  defstruct status: "",
            root: "",
            bare: false,
            head_sha: "",
            head_branch: "",
            head_ahead: 0,
            head_behind: 0,
            unstaged_files: 0,
            unstaged_insertions: 0,
            unstaged_deletions: 0,
            staged_files: 0,
            staged_insertions: 0,
            staged_deletions: 0,
            untracked_files: 0,
            stash_size: 0,
            assume_unchanged_files: 0,
            submodule_uninitialized: 0,
            submodule_out_of_sync: 0,
            submodule_merge_conflicts: 0

  def repo_stats(path) do
    # Shell out to git to get additional information for those parts that are
    # faster using the vanilla git binary instead of using the Rust bindings for
    # libgit2. We use Task.async to basically "background" these processes while
    # we do other work.
    task_get_diff_stats = Task.async(fn -> diff!(path) end)
    task_get_diff_cached_stats = Task.async(fn -> diff_cached!(path) end)

    json = LH.Lightning.repo_stats(path)
    git_repo_stats = Poison.decode!(json, as: %LH.GitRepo{})

    # For this task, we use the root of the repo (calculated by
    # LH.Lightning.repo_stats) because it is sensitive to the PWD in which the
    # command runs.
    task_get_untracked_files = Task.async(fn -> untracked_files!(git_repo_stats.root) end)
    task_get_submodule_status = Task.async(fn -> submodule_status!(git_repo_stats.root) end)

    diff_stats = Task.await(task_get_diff_stats)
    diff_cached_stats = Task.await(task_get_diff_cached_stats)
    untracked_files = Task.await(task_get_untracked_files)
    submodule_status = Task.await(task_get_submodule_status)

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

    submodules = %{
      submodule_uninitialized: Map.get(submodule_status, "-", 0),
      submodule_out_of_sync: Map.get(submodule_status, "+", 0),
      submodule_merge_conflicts: Map.get(submodule_status, "U", 0)
    }

    git_repo_stats = Map.merge(git_repo_stats, unstaged)
    git_repo_stats = Map.merge(git_repo_stats, staged)
    git_repo_stats = Map.replace!(git_repo_stats, :untracked_files, untracked_files)
    git_repo_stats = Map.merge(git_repo_stats, submodules)

    %{git_repo_stats | status: "FINISHED"}
  end

  defp diff!(path) do
    case System.cmd("git", ["diff", "--shortstat"], cd: path) do
      {shortstat, 0} ->
        to_map(shortstat)

      {_, code} ->
        raise RuntimeError, "'git diff --shortstat' failed with code #{code}"
    end
  end

  defp diff_cached!(path) do
    case System.cmd("git", ["diff", "--cached", "--shortstat"], cd: path) do
      {shortstat, 0} ->
        to_map(shortstat)

      {_, code} ->
        raise RuntimeError, "'git diff --cached --shortstat' failed with code #{code}"
    end
  end

  # Count untracked files.
  defp untracked_files!(path) do
    case System.cmd("git", ["ls-files", "--others", "--exclude-standard"], cd: path) do
      {untracked_files, 0} ->
        # Count each line (assume each file is on its own line). Discard blank
        # lines.
        untracked_files
        |> discard_trailing_empty_lines()
        |> Kernel.length()

      {_, code} ->
        raise RuntimeError, "'git ls-files --others --exclude-standard' failed with code #{code}"
    end
  end

  # Get statuses of all submodules for this repo. This mirrors the "git
  # submodule status" command.
  defp submodule_status!(path) do
    case System.cmd("git", ["submodule", "status"], cd: path) do
      {lines, 0} ->
        lines
        |> discard_trailing_empty_lines()
        |> Enum.map(&%{type: String.first(&1)})
        |> Enum.frequencies_by(& &1.type)

      {_, code} ->
        raise RuntimeError, "'git submodule status' failed with code #{code}"
    end
  end

  # Get root directory of given path if it is a Git repo.
  def get_repo_root(path) do
    case System.cmd("git", ["rev-parse", "--show-toplevel"], cd: path) do
      {output, 0} ->
        cond do
          String.length(output) > 0 ->
            {:ok, String.trim_trailing(output)}

          true ->
            {:error, :no_repo_id}
        end

      {_, code} ->
        {:error, "'git rev-parse --show-toplevel' failed with code #{code} for path '#{path}'"}
    end
  end

  def discard_trailing_empty_lines(lines) do
    lines
    |> String.split(["\n", "\r", "\r\n"])
    |> Enum.take_while(fn x -> String.trim(x) |> String.length() > 0 end)
  end

  defp to_map(shortstat) do
    # The shortstat string looks like this:
    #   " 3 files changed, 9 insertions(+), 3 deletions(-)"
    # We split by the comma, then take the first 2 words.
    pairs =
      shortstat
      |> String.split(",")
      # Take first 2 words. E.g., ["3", "files"], ["9", "insertions(+)"].
      |> Enum.map(&(&1 |> String.trim() |> String.split(" ") |> Enum.take(2)))
      |> Enum.filter(&(Kernel.length(&1) == 2))
      # Convert ["9", "insertions(+)"] into {:insertions, 9}.
      |> Enum.map(&sanitize_to_pair!(&1))

    Map.new(pairs)
  end

  defp sanitize_to_pair!(x) do
    case x do
      [n, keyword] ->
        {
          # Recall that the keyword can look like "insertions(+)" or
          # "insertion(+)". We get the 2nd to last character to see what kind of
          # string it is, and then manually convert to an atom.
          case String.slice(keyword, -2..-2) do
            "+" -> :insertions
            "-" -> :deletions
            _ -> :files
          end,
          String.to_integer(n)
        }

      x ->
        raise RuntimeError, "cannot convert #{x} (not a 2-element list) to a tuple pair"
    end
  end
end
