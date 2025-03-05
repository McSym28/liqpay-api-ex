defmodule LiqPayAPI.Confirmation.ThreeDS.Request do
  @moduledoc """
  Provides struct and type for a Confirmation.ThreeDS.Request
  """

  @behaviour OpenAPIClient.Schema

  @type t :: %__MODULE__{
          action: :confirm,
          confirm_token: String.t(),
          public_key: String.t(),
          version: 3
        }
  @type types :: :t

  @enforce_keys [:confirm_token]
  defstruct [:confirm_token, :public_key, action: :confirm, version: 3]

  @doc false
  @impl OpenAPIClient.Schema
  @spec __fields__(types()) :: keyword(OpenAPIClient.Schema.schema_type())
  def __fields__(:t) do
    [
      action: {"action", {:enum, confirm: "confirm"}},
      confirm_token: {"confirm_token", {:string, :generic}},
      public_key: {"public_key", {:string, :generic}},
      version: {"version", {:enum, [3]}}
    ]
  end
end
