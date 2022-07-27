import Config

config :logger, :console,
  format: "[$level] $message $metadata\n",
  metadata: [:error_code, :file, :line, :mfa]

import_config "#{config_env()}.exs"
