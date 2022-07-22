defmodule LH.Git do
  @moduledoc """
  Call Git binary on the system to get some information from it. This is used in
  cases where the equivalent libgit2 Rust binding is too slow.
  """
  @derive [Poison.Encoder]
  defstruct root: "",
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
            assume_unchanged_files: 0

  def diff(path) do
    case System.cmd("git", ["diff", "--shortstat"], cd: path) do
      {shortstat, 0} ->
        to_map(shortstat)

      {_, code} ->
        raise RuntimeError, "'git diff --shortstat' failed with code #{code}"
    end
  end

  def diff_cached(path) do
    case System.cmd("git", ["diff", "--cached", "--shortstat"], cd: path) do
      {shortstat, 0} ->
        to_map(shortstat)

      {_, code} ->
        raise RuntimeError, "'git diff --cached --shortstat' failed with code #{code}"
    end
  end

  # Count untracked files.
  def untracked_files(path) do
    case System.cmd("git", ["ls-files", "--others", "--exclude-standard"], cd: path) do
      {untracked_files, 0} ->
        # Count each line (assume each file is on its own line). Discard blank
        # lines.
        untracked_files
        |> String.split(["\n", "\r", "\r\n"])
        |> Enum.take_while(fn x -> String.trim(x) |> String.length() > 0 end)
        |> Kernel.length()

      {_, code} ->
        raise RuntimeError, "'git ls-files --others --exclude-standard' failed with code #{code}"
    end
  end

  def to_map(shortstat) do
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
      |> Enum.map(&sanitize_to_pair(&1))

    Map.new(pairs)
  end

  def sanitize_to_pair(x) do
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
