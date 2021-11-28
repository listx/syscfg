defmodule LH.Router do
  require Logger
  use Plug.Router

  plug(Plug.Logger)
  plug(:match)
  plug(Plug.Parsers, parsers: [:json], json_decoder: Jason)
  plug(:dispatch)

  get "/" do
    msg = LH.Lightning.hfr()
    send_resp(conn, 200, msg)
  end

  get "/shutdown" do
    send_resp(conn, 200, "Shutting down...")
    shutdown()
  end

  post "/path-shorten" do
    {status, body} =
      case conn.body_params do
        %{"path" => path} -> {200, path_shorten(path)}
        _ -> {422, missing_path()}
      end

    send_resp(conn, status, body)
  end

  # Strings in Elixir are represented as binaries, so we use is_binary/1 instead
  # of is_string/1.
  defp path_shorten(path) when is_binary(path) do
    {status, msg} = Cachex.get(:path_cache, path)

    {msg_final, cached_status} =
      if status == :error || msg == nil do
        home_dir = System.get_env("HOME")
        path_aliases_file = home_dir <> "/syscfg/zsh/path-aliases"

        msg =
          LH.Lightning.shorten(
            path,
            path_aliases_file
          )

        Cachex.put(:path_cache, path, msg)
        {msg, :MIS}
      else
        {msg, :HIT}
      end

    Logger.info("/path-shorten: (#{cached_status}) #{inspect(path)} => #{inspect(msg_final)}")

    Jason.encode!(%{path_shortened: msg_final})
  end

  defp path_shorten(_) do
    Jason.encode!(%{error: "Expected Payload: { 'path': '...' }"})
  end

  defp missing_path do
    Jason.encode!(%{error: "Expected Payload: { 'path': '...' }"})
  end

  defp shutdown do
    System.stop(0)
  end

  # A catchall route to match anything else that does not line up to the
  # expected endpoints above.
  match _ do
    send_resp(conn, 404, "Oops!")
  end
end
