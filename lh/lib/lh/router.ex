defmodule LH.Router do
  require Logger
  use Plug.Router

  plug(Plug.Logger)
  plug(:match)
  plug(Plug.Parsers, parsers: [:json], json_decoder: Jason)
  plug(:dispatch)

  get "/" do
    msg = LH.Lightning.hfr()

    conn
    |> send_resp(200, msg)
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

  post "/path-shorten" do
    {status, body} =
      case conn.body_params do
        %{"name" => path, "aliases_raw" => aliases_raw, "substitutions" => subs} ->
          {200, path_shorten(path, aliases_raw, subs)}

        _ ->
          {422, missing_path()}
      end

    conn
    |> put_resp_header("content-type", "application/json;charset=utf-8")
    |> send_resp(status, body)
    |> halt()
  end

  # Strings in Elixir are represented as binaries, so we use is_binary/1 instead
  # of is_string/1.
  defp path_shorten(path, aliases_raw, subs)
       when is_binary(path) and is_binary(aliases_raw) and is_map(subs) do
    {status, msg} = Cachex.get(:path_cache, {path, aliases_raw, subs})

    {msg_final, cached_status} =
      if status == :error || msg == nil do
        msg =
          LH.Lightning.shorten(
            path,
            aliases_raw,
            subs
          )

        Cachex.put(:path_cache, {path, aliases_raw, subs}, msg)
        {msg, :MIS}
      else
        {msg, :HIT}
      end

    Logger.info("/path-shorten: (#{cached_status}) #{inspect(path)} => #{inspect(msg_final)}")

    Jason.encode!(%{path_shortened: msg_final})
  end

  defp path_shorten(path, aliases_raw, subs) do
    if not is_binary(path) do
      Logger.error("/path-shorten: path is not a string: #{path}")
    end

    if not is_binary(aliases_raw) do
      Logger.error("/path-shorten: aliases_raw is not a string: #{aliases_raw}")
    end

    if not is_map(subs) do
      Logger.error("/path-shorten: subs is not a map: #{subs}")
    end

    Jason.encode!(%{
      error: "bad arguments"
    })
  end

  defp missing_path do
    Jason.encode!(%{
      error: "unknown path"
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
