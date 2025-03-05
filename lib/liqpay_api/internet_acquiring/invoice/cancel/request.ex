defmodule LiqPayAPI.InternetAcquiring.Invoice.Cancel.Request do
  @moduledoc """
  Provides struct and type for a InternetAcquiring.Invoice.Cancel.Request
  """

  @behaviour OpenAPIClient.Schema

  @type t :: %__MODULE__{
          action: :invoice_cancel,
          order_id: String.t(),
          public_key: String.t(),
          version: 3
        }
  @type types :: :t

  @enforce_keys [:order_id]
  defstruct [:order_id, :public_key, action: :invoice_cancel, version: 3]

  @doc false
  @impl OpenAPIClient.Schema
  @spec __fields__(types()) :: keyword(OpenAPIClient.Schema.schema_type())
  def __fields__(:t) do
    [
      action: {"action", {:enum, invoice_cancel: "invoice_cancel"}},
      order_id: {"order_id", {:string, :generic}},
      public_key: {"public_key", {:string, :generic}},
      version: {"version", {:enum, [3]}}
    ]
  end
end
