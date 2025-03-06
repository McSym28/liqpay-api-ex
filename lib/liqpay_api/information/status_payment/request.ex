defmodule LiqPayAPI.Information.StatusPayment.Request do
  @moduledoc """
  Provides struct and type for a Information.StatusPayment.Request
  """

  @behaviour OpenAPIClient.Schema

  @type t :: %__MODULE__{
          action: :status,
          order_id: String.t(),
          public_key: String.t(),
          version: 3
        }
  @type types :: :t

  @enforce_keys [:order_id]
  defstruct [:order_id, :public_key, action: :status, version: 3]

  @doc false
  @impl OpenAPIClient.Schema
  @spec __fields__(types()) :: keyword(OpenAPIClient.Schema.field_type())
  def __fields__(:t) do
    [
      action: {"action", {:enum, status: "status"}},
      order_id: {"order_id", {:string, :generic}},
      public_key:
        {"public_key", {:string, :generic},
         fn -> Application.get_env(:liqpay_api_ex, :public_key) end},
      version: {"version", {:enum, [3]}}
    ]
  end
end
