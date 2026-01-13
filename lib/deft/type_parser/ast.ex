defmodule Deft.TypeParser.AST do
  @moduledoc """
  Intermediate AST representation for parsed type expressions.

  These structs preserve span information from the source for error reporting,
  and can be converted to either runtime `Type.t()` structs or quoted AST.

  ## Span Format

  Spans are stored as `{line, column}` tuples where column may be `nil` if
  not available in the source metadata.
  """

  @type span :: {line :: pos_integer(), column :: pos_integer() | nil} | nil

  defmodule Primitive do
    @moduledoc """
    A primitive type: integer, float, number, boolean, atom, binary, top, or bottom.
    """
    @type kind :: :integer | :float | :number | :boolean | :atom | :binary | :top | :bottom

    @type t :: %__MODULE__{
            kind: kind(),
            span: Deft.TypeParser.AST.span()
          }

    @enforce_keys [:kind]
    defstruct [:kind, :span]
  end

  defmodule Tuple do
    @moduledoc """
    A fixed-size tuple type with known element types.
    """
    @type t :: %__MODULE__{
            elements: [Deft.TypeParser.AST.t()],
            span: Deft.TypeParser.AST.span()
          }

    @enforce_keys [:elements]
    defstruct [:elements, :span]
  end

  defmodule Union do
    @moduledoc """
    A union of two types: `left | right`.
    """
    @type t :: %__MODULE__{
            left: Deft.TypeParser.AST.t(),
            right: Deft.TypeParser.AST.t(),
            span: Deft.TypeParser.AST.span()
          }

    @enforce_keys [:left, :right]
    defstruct [:left, :right, :span]
  end

  defmodule List do
    @moduledoc """
    A list type with a known element type: `[element]`.
    """
    @type t :: %__MODULE__{
            element: Deft.TypeParser.AST.t(),
            span: Deft.TypeParser.AST.span()
          }

    @enforce_keys [:element]
    defstruct [:element, :span]
  end

  defmodule Function do
    @moduledoc """
    A function type: `(inputs -> output)`.
    """
    @type t :: %__MODULE__{
            inputs: [Deft.TypeParser.AST.t()],
            output: Deft.TypeParser.AST.t(),
            span: Deft.TypeParser.AST.span()
          }

    @enforce_keys [:inputs, :output]
    defstruct [:inputs, :output, :span]
  end

  defmodule Variable do
    @moduledoc """
    A type variable (single lowercase letter in signature context).
    """
    @type t :: %__MODULE__{
            name: atom(),
            span: Deft.TypeParser.AST.span()
          }

    @enforce_keys [:name]
    defstruct [:name, :span]
  end

  defmodule Alias do
    @moduledoc """
    A reference to a user-defined type alias.
    """
    @type t :: %__MODULE__{
            name: atom(),
            context: atom() | nil,
            span: Deft.TypeParser.AST.span()
          }

    @enforce_keys [:name]
    defstruct [:name, :context, :span]
  end

  defmodule Application do
    @moduledoc """
    A type application: `option(integer)`, `result(string, error)`.

    Represents a parameterized type being instantiated with concrete type arguments.
    """
    @type t :: %__MODULE__{
            name: atom(),
            args: [Deft.TypeParser.AST.t()],
            span: Deft.TypeParser.AST.span()
          }

    @enforce_keys [:name, :args]
    defstruct [:name, :args, :span]
  end

  defmodule Abstract do
    @moduledoc """
    An abstract type without element information: `list` or `tuple`.
    """
    @type kind :: :list | :tuple

    @type t :: %__MODULE__{
            kind: kind(),
            span: Deft.TypeParser.AST.span()
          }

    @enforce_keys [:kind]
    defstruct [:kind, :span]
  end

  @type t ::
          Primitive.t()
          | Tuple.t()
          | Union.t()
          | List.t()
          | Function.t()
          | Variable.t()
          | Alias.t()
          | Application.t()
          | Abstract.t()
end
