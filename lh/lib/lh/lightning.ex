defmodule LH.Lightning do
  use Rustler, otp_app: :lh, crate: "lh_lightning"

  # When your NIFs are loaded, they will override these functions.
  defp find_existing_parent_(_path), do: :erlang.nif_error(:nif_not_loaded)
  def is_git_index_file(_path), do: :erlang.nif_error(:nif_not_loaded)
  def is_git_repo(_path), do: :erlang.nif_error(:nif_not_loaded)
  def get_repo_id_(_path), do: :erlang.nif_error(:nif_not_loaded)
  def repo_stats(_path), do: :erlang.nif_error(:nif_not_loaded)
  def path_shorten(_path, _aliases), do: :erlang.nif_error(:nif_not_loaded)
  def paths_sort(_paths, _priorities_raw, _home), do: :erlang.nif_error(:nif_not_loaded)

  def find_existing_parent(path) do
    case find_existing_parent_(path) do
      "" -> {:error, :no_parent}
      parent -> {:ok, parent}
    end
  end

  def get_repo_id(path) do
    case get_repo_id_(path) do
      "" -> {:error, :no_repo_id}
      repo_id -> {:ok, repo_id}
    end
  end
end
