defmodule LiqPayAPI.InternetAcquiring.Widget.Request.Sender do
  @moduledoc """
  Provides struct and type for a InternetAcquiring.Widget.Request.Sender
  """

  @behaviour OpenAPIClient.Schema

  @type t :: %__MODULE__{
          sender_address: String.t() | nil,
          sender_city: String.t() | nil,
          sender_country_code: String.t() | nil,
          sender_first_name: String.t() | nil,
          sender_last_name: String.t() | nil,
          sender_postal_code: String.t() | nil
        }
  @type types :: :t

  defstruct [
    :sender_address,
    :sender_city,
    :sender_country_code,
    :sender_first_name,
    :sender_last_name,
    :sender_postal_code
  ]

  @doc false
  @impl OpenAPIClient.Schema
  @spec __fields__(types()) :: keyword(OpenAPIClient.Schema.field_type())
  def __fields__(:t) do
    [
      sender_address: {"sender_address", {:string, :generic}},
      sender_city: {"sender_city", {:string, :generic}},
      sender_country_code: {"sender_country_code", {:string, :generic}},
      sender_first_name: {"sender_first_name", {:string, :generic}},
      sender_last_name: {"sender_last_name", {:string, :generic}},
      sender_postal_code: {"sender_postal_code", {:string, :generic}}
    ]
  end
end
