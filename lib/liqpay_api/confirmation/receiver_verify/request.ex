defmodule LiqPayAPI.Confirmation.ReceiverVerify.Request do
  @moduledoc """
  Provides struct and type for a Confirmation.ReceiverVerify.Request
  """

  @behaviour OpenAPIClient.Schema

  @type t :: %__MODULE__{
          action: :confirm,
          confirm_token: String.t(),
          public_key: String.t(),
          receiver_first_name: String.t(),
          receiver_last_name: String.t(),
          version: 3
        }
  @type types :: :t

  @enforce_keys [:confirm_token, :receiver_first_name, :receiver_last_name]
  defstruct [
    :confirm_token,
    :public_key,
    :receiver_first_name,
    :receiver_last_name,
    action: :confirm,
    version: 3
  ]

  @doc false
  @impl OpenAPIClient.Schema
  @spec __fields__(types()) :: keyword(OpenAPIClient.Schema.schema_type())
  def __fields__(:t) do
    [
      action: {"action", {:enum, confirm: "confirm"}},
      confirm_token: {"confirm_token", {:string, :generic}},
      public_key: {"public_key", {:string, :generic}},
      receiver_first_name: {"receiver_first_name", {:string, :generic}},
      receiver_last_name: {"receiver_last_name", {:string, :generic}},
      version: {"version", {:enum, [3]}}
    ]
  end
end
