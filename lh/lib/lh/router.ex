defmodule LH.Router do
  require Logger
  use Plug.Router

  plug(Plug.Logger)
  plug(:match)
  plug(Plug.Parsers, parsers: [:json], json_decoder: Jason)
  plug(:dispatch)

  get "/" do
    conn
    |> send_resp(200, "hello")
    |> halt()
  end

  get "/ping" do
    conn
    |> send_resp(200, "")
    |> halt()
  end

  get "/shutdown" do
    conn
    |> send_resp(200, "Shutting down...")
    |> halt()

    shutdown()
  end

  post "/prompt-info" do
    {status, body} =
      case conn.body_params do
        %{"path" => path, "aliases" => aliases} ->
          {200, prompt_info(path, aliases)}

        _ ->
          {422, bad_request_body()}
      end

    conn
    |> put_resp_header("content-type", "application/json;charset=utf-8")
    |> send_resp(status, body)
    |> halt()
  end

  post "/git-info" do
    {status, body} =
      case conn.body_params do
        %{"path" => path} ->
          {200, repo_stats(path)}

        _ ->
          {422, bad_request_body()}
      end

    conn
    |> put_resp_header("content-type", "application/json;charset=utf-8")
    |> send_resp(status, body)
    |> halt()
  end

  post "/path-shorten" do
    {status, body} =
      case conn.body_params do
        %{"path" => path, "aliases" => aliases} ->
          {200, path_shorten(path, aliases)}

        _ ->
          {422, bad_request_body()}
      end

    conn
    |> put_resp_header("content-type", "application/json;charset=utf-8")
    |> send_resp(status, body)
    |> halt()
  end

  post "/paths-sort" do
    {status, body} =
      case conn.body_params do
        %{"paths" => paths, "priorities_raw" => priorities_raw, "home" => home} ->
          {200, paths_sort(paths, priorities_raw, home)}

        _ ->
          {422, bad_request_body()}
      end

    conn
    |> put_resp_header("content-type", "application/json;charset=utf-8")
    |> send_resp(status, body)
    |> halt()
  end

  defp prompt_info(path, aliases)
       when is_binary(path) and is_binary(aliases) do
    prompt = LH.Prompt.generate(path, aliases)

    Logger.info("/prompt-info: #{inspect(path)} => #{inspect(prompt)}")

    Poison.encode!(prompt)
  end

  defp prompt_info(path, aliases) do
    if not is_binary(path) do
      Logger.error("/prompt-info: path is not a string: #{path}")
    end

    if not is_binary(aliases) do
      Logger.error("/prompt-info: aliases is not a string: #{aliases}")
    end

    Jason.encode!(%{
      error: "bad arguments"
    })
  end

  # Strings in Elixir are represented as binaries, so we use is_binary/1 instead
  # of is_string/1.
  defp repo_stats(path)
       when is_binary(path) do
    git_repo_stats = LH.Git.repo_stats(path)

    Logger.info("/git-info: #{inspect(path)} => #{inspect(git_repo_stats)}")

    Poison.encode!(git_repo_stats)
  end

  defp repo_stats(path) do
    if not is_binary(path) do
      Logger.error("/git-info: path is not a string: #{path}")
    end

    Jason.encode!(%{
      error: "bad arguments"
    })
  end

  # Strings in Elixir are represented as binaries, so we use is_binary/1 instead
  # of is_string/1.
  defp path_shorten(path, aliases)
       when is_binary(path) and is_binary(aliases) do
    {path_short, cached_status} = LH.Path.path_shorten(path, aliases)

    Logger.info("/path-shorten: (#{cached_status}) #{inspect(path)} => #{inspect(path_short)}")

    Jason.encode!(%{path_shortened: path_short})
  end

  defp path_shorten(path, aliases) do
    if not is_binary(path) do
      Logger.error("/path-shorten: path is not a string: #{path}")
    end

    if not is_binary(aliases) do
      Logger.error("/path-shorten: aliases is not a string: #{aliases}")
    end

    Jason.encode!(%{
      error: "bad arguments"
    })
  end

  defp paths_sort(paths, priorities_raw, home)
       when is_binary(paths) and is_binary(priorities_raw) and is_binary(home) do
    {status, msg} = Cachex.get(:paths_sort_cache, {paths, priorities_raw, home})

    {msg_final, cached_status} =
      if status == :error || msg == nil do
        msg = LH.Lightning.paths_sort(paths, priorities_raw, home)

        Cachex.put(:paths_sort_cache, {paths, priorities_raw, home}, msg)
        {msg, :MIS}
      else
        {msg, :HIT}
      end

    Logger.info("/paths-sort: (#{cached_status}) #{inspect(paths)} => #{inspect(msg_final)}")

    Jason.encode!(%{paths_sorted: msg_final})
  end

  defp paths_sort(paths, priorities_raw, home) do
    if not is_binary(paths) do
      Logger.error("/path-shorten: paths is not a string: #{paths}")
    end

    if not is_binary(priorities_raw) do
      Logger.error("/path-shorten: priorities_raw is not a string: #{priorities_raw}")
    end

    if not is_binary(home) do
      Logger.error("/path-shorten: home is not a string: #{home}")
    end

    Jason.encode!(%{
      error: "bad arguments"
    })
  end

  defp bad_request_body do
    Jason.encode!(%{
      error: "bad_request_body"
    })
  end

  defp shutdown do
    System.stop(0)
  end

  # A catchall route to match anything else that does not line up to the
  # expected endpoints above.
  match _ do
    conn
    |> send_resp(404, "Oops!")
    |> halt()
  end
end
