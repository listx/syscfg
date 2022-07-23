defmodule LH.Lightning do
  use Rustler, otp_app: :lh, crate: "lh_lightning"

  # When your NIFs are loaded, they will override these functions.
  def repo_stats(_path), do: :erlang.nif_error(:nif_not_loaded)
  def path_shorten(_path, _aliases), do: :erlang.nif_error(:nif_not_loaded)
  def paths_sort(_paths, _priorities_raw, _home), do: :erlang.nif_error(:nif_not_loaded)
end
