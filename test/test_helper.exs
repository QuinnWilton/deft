ExUnit.start()

# Configure ExUnit
ExUnit.configure(
  exclude: [:skip, :slow],
  include: [],
  max_cases: System.schedulers_online() * 2
)

# Load support files
Code.require_file("support/type_assertions.ex", __DIR__)
Code.require_file("support/ast_builders.ex", __DIR__)
Code.require_file("support/case_templates.ex", __DIR__)
