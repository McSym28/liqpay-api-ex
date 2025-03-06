defmodule LiqPayAPI.Public.Exchange.Response do
  @moduledoc """
  Provides struct and type for a Public.Exchange.Response
  """

  @behaviour OpenAPIClient.Schema

  @type t :: %__MODULE__{
          base_ccy: String.t() | nil,
          buy: String.t() | nil,
          ccy: String.t() | nil,
          sale: String.t() | nil
        }
  @type types :: :t

  defstruct [:base_ccy, :buy, :ccy, :sale]

  @doc false
  @impl OpenAPIClient.Schema
  @spec __fields__(types()) :: keyword(OpenAPIClient.Schema.field_type())
  def __fields__(:t) do
    [
      base_ccy: {"base_ccy", {:string, :generic}},
      buy: {"buy", {:string, :generic}},
      ccy: {"ccy", {:string, :generic}},
      sale: {"sale", {:string, :generic}}
    ]
  end
end
