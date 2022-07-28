defmodule LH.Git do
  @moduledoc """
  Git repo watchers are created dynamically during runtime. This module
  supervises these watchers so that they are restarted if they should fail.
  """

  require Logger

  def start_link() do
    Logger.info("Starting Git watcher dynamic supervisor")
    DynamicSupervisor.start_link(name: __MODULE__, strategy: :one_for_one)
  end

  def child_spec(_arg) do
    %{
      id: __MODULE__,
      start: {__MODULE__, :start_link, []},
      type: :supervisor
    }
  end

  def start_watcher(repo_id) do
    case start_child(repo_id) do
      {:ok, pid} -> pid
      {:error, {:already_started, pid}} -> pid
      unknown -> Logger.warn("start_watcher failed: #{inspect(unknown)}")
    end
  end

  defp start_child(repo_id) do
    # We pass in the repo_path (String) as an argument to the start_link/1
    # function of LH.GitWatcher.
    DynamicSupervisor.start_child(__MODULE__, {LH.GitWatcher, repo_id})
  end
end
