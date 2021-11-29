# TODO: Add some basic tests for known inputs/outputs.
# See
# https://dev.to/jonlunsford/elixir-building-a-small-json-endpoint-with-plug-cowboy-and-poison-1826
# for an idea on testing endpoints.

defmodule LHTest do
  use ExUnit.Case
  doctest LH

  test "greets the world" do
    assert LH.hello() == :world
  end
end
