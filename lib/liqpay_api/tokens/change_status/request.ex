defmodule LiqPayAPI.Tokens.ChangeStatus.Request do
  @moduledoc """
  Provides struct and type for a Tokens.ChangeStatus.Request
  """

  @behaviour OpenAPIClient.Schema

  @type t :: %__MODULE__{
          action: :token_update,
          card_token: String.t(),
          card_token_action: :delete | :suspend | :unsuspend,
          public_key: String.t(),
          version: 3
        }
  @type types :: :t

  @enforce_keys [:card_token, :card_token_action]
  defstruct [:card_token, :card_token_action, :public_key, action: :token_update, version: 3]

  @doc false
  @impl OpenAPIClient.Schema
  @spec __fields__(types()) :: keyword(OpenAPIClient.Schema.field_type())
  def __fields__(:t) do
    [
      action: {"action", {:enum, token_update: "token_update"}},
      card_token: {"card_token", {:string, :generic}},
      card_token_action:
        {"card_token_action",
         {:enum, delete: "DELETE", suspend: "SUSPEND", unsuspend: "UNSUSPEND"}},
      public_key:
        {"public_key", {:string, :generic},
         fn -> Application.get_env(:liqpay_api_ex, :public_key) end},
      version: {"version", {:enum, [3]}}
    ]
  end
end
