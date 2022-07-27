defmodule LH.Prompt do
  @moduledoc """
  Module to generate a shell prompt.
  """

  @derive [Poison.Encoder]
  defstruct path_short: "", git_repo_stats: %LH.GitRepo{}

  require Logger

  def generate(path, aliases) do
    # Start the Git watcher for this path. This is idempotent and will not spawn
    # a new watcher if one already exists for this path.
    repo_id = LH.Lightning.get_repo_id(path)

    # It could be that the given path is not a Git repo at all.
    git_repo_stats =
      if String.length(repo_id) > 0 do
        _pid = LH.Git.start_watcher(repo_id)
        LH.GitWatcher.get_repo_stats(repo_id)
      else
        Logger.info("SKIPPING: path #{path} is not a Git repo")
        %LH.GitRepo{}
      end

    {path_short, cached_status} = LH.Path.path_shorten(path, aliases)

    Logger.info("(path cache: #{cached_status}) #{inspect(path)} => #{inspect(path_short)}")

    %LH.Prompt{
      git_repo_stats: git_repo_stats,
      path_short: path_short
    }
  end
end
