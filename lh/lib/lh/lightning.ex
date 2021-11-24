defmodule LH.Lightning do
  use Rustler, otp_app: :lh, crate: "lh_lightning"

  # When your NIF is loaded, it will override this function.
  def add(_a, _b), do: :erlang.nif_error(:nif_not_loaded)
  def hfr(), do: :erlang.nif_error(:nif_not_loaded)
  def shorten(_path, _path_aliases_file), do: :erlang.nif_error(:nif_not_loaded)
end
