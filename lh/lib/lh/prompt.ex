defmodule LH.Prompt do
  @moduledoc """
  Module to generate a shell prompt.
  """

  @derive [Poison.Encoder]
  defstruct path_short: "", git_repo_stats: %LH.Git{}

  require Logger

  def generate(path, aliases) do
    git_repo_stats =
      if LH.Lightning.is_git_repo(path) do
        LH.Git.repo_stats(path)
      else
        %LH.Git{}
      end

    {path_short, cached_status} = LH.Path.path_shorten(path, aliases)
    Logger.info("(#{cached_status}) #{inspect(path)} => #{inspect(path_short)}")

    %LH.Prompt{
      git_repo_stats: git_repo_stats,
      path_short: path_short
    }
  end
end
