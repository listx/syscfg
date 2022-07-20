defmodule LH.Lightning do
  use Rustler, otp_app: :lh, crate: "lh_lightning"

  # When your NIF is loaded, it will override this function.
  def add(_a, _b), do: :erlang.nif_error(:nif_not_loaded)
  def hfr(), do: :erlang.nif_error(:nif_not_loaded)
  def git_info(_path), do: :erlang.nif_error(:nif_not_loaded)
  def path_shorten(_path, _aliases_raw, _subs), do: :erlang.nif_error(:nif_not_loaded)
  def paths_sort(_paths, _priorities_raw, _home), do: :erlang.nif_error(:nif_not_loaded)
end
