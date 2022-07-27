defmodule LH.Prompt do
  @moduledoc """
  Module to generate a shell prompt.
  """

  @derive [Poison.Encoder]
  defstruct path_short: "", git_repo_stats: %LH.GitRepo{}

  require Logger

  def generate(path, aliases) do
    git_repo_stats =
      case LH.Lightning.get_repo_id(path) do
        {:error, :no_repo_id} ->
          Logger.info("SKIPPING: path #{path} is not a Git repo")
          %LH.GitRepo{}

        {:ok, repo_id} ->
          LH.GitWatcher.get_repo_stats(repo_id)
      end

    {path_short, cached_status} = LH.Path.path_shorten(path, aliases)

    Logger.info("(path cache: #{cached_status}) #{inspect(path)} => #{inspect(path_short)}")

    %LH.Prompt{
      git_repo_stats: git_repo_stats,
      path_short: path_short
    }
  end
end
