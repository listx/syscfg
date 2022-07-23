defmodule LH.Path do
  @moduledoc """
  Caching wrapper around path shortening function.
  """
  def path_shorten(path, aliases) do
    {status, msg} = Cachex.get(:path_shorten_cache, {path, aliases})

    if status == :error || msg == nil do
      msg =
        LH.Lightning.path_shorten(
          path,
          aliases
        )

      Cachex.put(:path_shorten_cache, {path, aliases}, msg)
      {msg, :MIS}
    else
      {msg, :HIT}
    end
  end
end
