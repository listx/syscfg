defmodule LHTest do
  use ExUnit.Case
  doctest LH

  test "greets the world" do
    assert LH.hello() == :world
  end
end
