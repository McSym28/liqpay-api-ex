defmodule LiqPayAPI.Confirmation.CVV.Request do
  @moduledoc """
  Provides struct and type for a Confirmation.CVV.Request
  """

  @behaviour OpenAPIClient.Schema

  @type t :: %__MODULE__{
          action: :confirm,
          card_cvv: String.t(),
          confirm_token: String.t(),
          public_key: String.t(),
          version: 3
        }
  @type types :: :t

  @enforce_keys [:card_cvv, :confirm_token]
  defstruct [:card_cvv, :confirm_token, :public_key, action: :confirm, version: 3]

  @doc false
  @impl OpenAPIClient.Schema
  @spec __fields__(types()) :: keyword(OpenAPIClient.Schema.schema_type())
  def __fields__(:t) do
    [
      action: {"action", {:enum, confirm: "confirm"}},
      card_cvv: {"card_cvv", {:string, :generic}},
      confirm_token: {"confirm_token", {:string, :generic}},
      public_key: {"public_key", {:string, :generic}},
      version: {"version", {:enum, [3]}}
    ]
  end
end
