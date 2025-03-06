defmodule LiqPayAPI.InternetAcquiring.CardPayment.Request.Sender do
  @moduledoc """
  Provides struct and type for a InternetAcquiring.CardPayment.Request.Sender
  """

  @behaviour OpenAPIClient.Schema

  @type t :: %__MODULE__{
          sender_address: String.t(),
          sender_city: String.t(),
          sender_country_code: String.t(),
          sender_email: String.t() | nil,
          sender_first_name: String.t() | nil,
          sender_last_name: String.t() | nil,
          sender_postal_code: String.t(),
          sender_shipping_state: String.t() | nil,
          sender_state: String.t() | nil
        }
  @type types :: :t

  @enforce_keys [:sender_address, :sender_city, :sender_country_code, :sender_postal_code]
  defstruct [
    :sender_address,
    :sender_city,
    :sender_country_code,
    :sender_email,
    :sender_first_name,
    :sender_last_name,
    :sender_postal_code,
    :sender_shipping_state,
    :sender_state
  ]

  @doc false
  @impl OpenAPIClient.Schema
  @spec __fields__(types()) :: keyword(OpenAPIClient.Schema.field_type())
  def __fields__(:t) do
    [
      sender_address: {"sender_address", {:string, :generic}},
      sender_city: {"sender_city", {:string, :generic}},
      sender_country_code: {"sender_country_code", {:string, :generic}},
      sender_email: {"sender_email", {:string, :generic}},
      sender_first_name: {"sender_first_name", {:string, :generic}},
      sender_last_name: {"sender_last_name", {:string, :generic}},
      sender_postal_code: {"sender_postal_code", {:string, :generic}},
      sender_shipping_state: {"sender_shipping_state", {:string, :generic}},
      sender_state: {"sender_state", {:string, :generic}}
    ]
  end
end
