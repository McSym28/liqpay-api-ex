defmodule LiqPayAPI.Confirmation.SenderVerify.Request do
  @moduledoc """
  Provides struct and type for a Confirmation.SenderVerify.Request
  """

  @behaviour OpenAPIClient.Schema

  @type t :: %__MODULE__{
          action: :confirm,
          confirm_token: String.t(),
          public_key: String.t(),
          sender_address: String.t(),
          sender_city: String.t(),
          sender_country_code: String.t(),
          sender_first_name: String.t(),
          sender_last_name: String.t() | nil,
          sender_postal_code: String.t(),
          version: 3
        }
  @type types :: :t

  @enforce_keys [
    :confirm_token,
    :sender_address,
    :sender_city,
    :sender_country_code,
    :sender_first_name,
    :sender_postal_code
  ]
  defstruct [
    :confirm_token,
    :public_key,
    :sender_address,
    :sender_city,
    :sender_country_code,
    :sender_first_name,
    :sender_last_name,
    :sender_postal_code,
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
      sender_address: {"sender_address", {:string, :generic}},
      sender_city: {"sender_city", {:string, :generic}},
      sender_country_code: {"sender_country_code", {:string, :generic}},
      sender_first_name: {"sender_first_name", {:string, :generic}},
      sender_last_name: {"sender_last_name", {:string, :generic}},
      sender_postal_code: {"sender_postal_code", {:string, :generic}},
      version: {"version", {:enum, [3]}}
    ]
  end
end
