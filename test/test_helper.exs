ExUnit.start()

# Configure ExUnit
ExUnit.configure(
  exclude: [:skip, :slow],
  include: [],
  max_cases: System.schedulers_online() * 2
)

# Support files are loaded via elixirc_paths in mix.exs
