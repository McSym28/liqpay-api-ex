defmodule LiqPayAPI.InternetAcquiring.Invoice.Units.Response.SingleLanguage do
  @moduledoc """
  Provides struct and type for a InternetAcquiring.Invoice.Units.Response.SingleLanguage
  """

  @behaviour OpenAPIClient.Schema

  @type t :: %__MODULE__{
          full_name: String.t() | nil,
          id: integer | nil,
          short_name: String.t() | nil
        }
  @type types :: :t

  defstruct [:full_name, :id, :short_name]

  @doc false
  @impl OpenAPIClient.Schema
  @spec __fields__(types()) :: keyword(OpenAPIClient.Schema.field_type())
  def __fields__(:t) do
    [
      full_name: {"full_name", {:string, :generic}},
      id: {"id", :integer},
      short_name: {"short_name", {:string, :generic}}
    ]
  end
end
