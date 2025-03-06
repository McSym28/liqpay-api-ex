defmodule LiqPayAPI.InternetAcquiring.Widget.Request.OneClickPayment do
  @moduledoc """
  Provides struct and type for a InternetAcquiring.Widget.Request.OneClickPayment
  """

  @behaviour OpenAPIClient.Schema

  @type t :: %__MODULE__{
          customer: String.t() | nil,
          customer_user_id: String.t() | nil,
          recurringbytoken: true | nil
        }
  @type types :: :t

  defstruct [:customer, :customer_user_id, :recurringbytoken]

  @doc false
  @impl OpenAPIClient.Schema
  @spec __fields__(types()) :: keyword(OpenAPIClient.Schema.field_type())
  def __fields__(:t) do
    [
      customer: {"customer", {:string, :generic}},
      customer_user_id: {"customer_user_id", {:string, :generic}},
      recurringbytoken: {"recurringbytoken", {:enum, true: "1"}}
    ]
  end
end
