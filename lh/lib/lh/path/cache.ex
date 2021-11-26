defmodule LH.Path.Cache do
  @moduledoc """
  Path Cache
  """
  @cache_id :path_cache

  def child_spec(_init_arg) do
    %{
      id: @cache_id,
      type: :supervisor,
      start:
        {Cachex, :start_link,
         [
           @cache_id,
           [
             limit: 256
           ]
         ]}
    }
  end
end
