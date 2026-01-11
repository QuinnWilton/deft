defmodule Deft.TypeCase do
  @moduledoc """
  ExUnit.CaseTemplate for tests requiring type system setup.

  Provides common aliases and a default context in the setup block.

  ## Usage

      defmodule MyTest do
        use Deft.TypeCase

        test "example" do
          # ctx is available from setup
          assert_subtype(Type.integer(), Type.number())
        end
      end
  """
  use ExUnit.CaseTemplate

  using do
    quote do
      alias Deft.AST
      alias Deft.Context
      alias Deft.Subtyping
      alias Deft.Type
      alias Deft.TypeChecker

      import Deft.TestHelpers.TypeAssertions
      import Deft.TestHelpers.ASTBuilders
    end
  end

  setup do
    %{ctx: Deft.Context.new(__ENV__, features: [:exhaustiveness_checking])}
  end
end

defmodule Deft.PropertyCase do
  @moduledoc """
  ExUnit.CaseTemplate for property-based tests.

  Provides ExUnitProperties, generators, and common aliases.

  ## Usage

      defmodule MyPropertyTest do
        use Deft.PropertyCase

        property "example" do
          check all type <- Generators.Types.primitive_type() do
            assert Subtyping.subtype_of?(type, type)
          end
        end
      end
  """
  use ExUnit.CaseTemplate

  using do
    quote do
      use ExUnitProperties

      alias Deft.Context
      alias Deft.Generators
      alias Deft.Subtyping
      alias Deft.Type
      alias Deft.TypeChecker

      import Deft.TestHelpers.TypeAssertions
    end
  end
end

defmodule Deft.IntegrationCase do
  @moduledoc """
  ExUnit.CaseTemplate for integration tests that use Deft.compile.

  Provides the Deft module and common assertions.
  """
  use ExUnit.CaseTemplate

  using do
    quote do
      require Deft

      alias Deft.Type

      import Deft.TestHelpers.TypeAssertions
    end
  end
end
