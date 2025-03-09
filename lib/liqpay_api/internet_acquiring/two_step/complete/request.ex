defmodule LiqPayAPI.InternetAcquiring.TwoStep.Complete.Request do
  @moduledoc """
  Provides struct and type for a InternetAcquiring.TwoStep.Complete.Request
  """

  @behaviour OpenAPIClient.Schema

  @type t :: %__MODULE__{
          action: :hold_completion,
          amount: number,
          order_id: String.t(),
          public_key: String.t(),
          rro_info: LiqPayAPI.InternetAcquiring.TwoStep.Complete.Request.RROInfo.t() | nil,
          split_tickets_only: boolean | nil,
          version: 3
        }
  @type types :: :t

  @enforce_keys [:amount, :order_id]
  defstruct [
    :amount,
    :order_id,
    :public_key,
    :rro_info,
    :split_tickets_only,
    action: :hold_completion,
    version: 3
  ]

  @doc false
  @impl OpenAPIClient.Schema
  @spec __fields__(types()) :: keyword(OpenAPIClient.Schema.field_type())
  def __fields__(:t) do
    [
      action: {"action", {:enum, hold_completion: "hold_completion"}},
      amount: {"amount", :number},
      order_id: {"order_id", {:string, :generic}},
      public_key:
        {"public_key", {:string, :generic},
         fn -> Application.get_env(:liqpay_api_ex, :public_key) end},
      rro_info: {"rro_info", {LiqPayAPI.InternetAcquiring.TwoStep.Complete.Request.RROInfo, :t}},
      split_tickets_only: {"split_tickets_only", :boolean},
      version: {"version", {:enum, [3]}}
    ]
  end
end
