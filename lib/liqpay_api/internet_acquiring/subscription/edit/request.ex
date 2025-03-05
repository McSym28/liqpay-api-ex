defmodule LiqPayAPI.InternetAcquiring.Subscription.Edit.Request do
  @moduledoc """
  Provides struct and type for a InternetAcquiring.Subscription.Edit.Request
  """

  @behaviour OpenAPIClient.Schema

  @type t :: %__MODULE__{
          action: :subscribe_update,
          amount: number,
          currency: :eur | :uah | :usd,
          description: String.t(),
          order_id: String.t(),
          public_key: String.t(),
          version: 3
        }
  @type types :: :t

  @enforce_keys [:amount, :currency, :description, :order_id]
  defstruct [
    :amount,
    :currency,
    :description,
    :order_id,
    :public_key,
    action: :subscribe_update,
    version: 3
  ]

  @doc false
  @impl OpenAPIClient.Schema
  @spec __fields__(types()) :: keyword(OpenAPIClient.Schema.schema_type())
  def __fields__(:t) do
    [
      action: {"action", {:enum, subscribe_update: "subscribe_update"}},
      amount: {"amount", :number},
      currency: {"currency", {:enum, eur: "EUR", uah: "UAH", usd: "USD"}},
      description: {"description", {:string, :generic}},
      order_id: {"order_id", {:string, :generic}},
      public_key: {"public_key", {:string, :generic}},
      version: {"version", {:enum, [3]}}
    ]
  end
end
