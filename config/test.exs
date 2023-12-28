import Config

# We don't run a server during test. If one is required,
# you can enable the server option below.
config :erl_phoenix, ErlPhoenixEndpoint,
  http: [ip: {127, 0, 0, 1}, port: 4002],
  secret_key_base: "kVexq6PPEb4kSvKGIDINl3nUFJalnaridi1pv+tpXRv3fs1MBsicDC5Jif7QKCOL",
  server: false

# Print only warnings and errors during test
config :logger, level: :warning

# Initialize plugs at runtime for faster test compilation
config :phoenix, :plug_init_mode, :runtime
