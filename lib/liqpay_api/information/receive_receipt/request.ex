defmodule LiqPayAPI.Information.ReceiveReceipt.Request do
  @moduledoc """
  Provides struct and type for a Information.ReceiveReceipt.Request
  """

  @behaviour OpenAPIClient.Schema

  @type t :: %__MODULE__{
          action: :ticket,
          email: String.t(),
          language: :en | :uk | nil,
          order_id: String.t(),
          payment_id: number | nil,
          public_key: String.t(),
          version: 3
        }
  @type types :: :t

  @enforce_keys [:email, :order_id]
  defstruct [:email, :language, :order_id, :payment_id, :public_key, action: :ticket, version: 3]

  @doc false
  @impl OpenAPIClient.Schema
  @spec __fields__(types()) :: keyword(OpenAPIClient.Schema.schema_type())
  def __fields__(:t) do
    [
      action: {"action", {:enum, ticket: "ticket"}},
      email: {"email", {:string, :generic}},
      language: {"language", {:enum, en: "en", uk: "uk"}},
      order_id: {"order_id", {:string, :generic}},
      payment_id: {"payment_id", :number},
      public_key: {"public_key", {:string, :generic}},
      version: {"version", {:enum, [3]}}
    ]
  end
end
