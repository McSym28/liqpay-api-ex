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
          version: 3
        }
  @type types :: :t

  @enforce_keys [:amount, :order_id]
  defstruct [:amount, :order_id, :public_key, :rro_info, action: :hold_completion, version: 3]

  @doc false
  @impl OpenAPIClient.Schema
  @spec __fields__(types()) :: keyword(OpenAPIClient.Schema.schema_type())
  def __fields__(:t) do
    [
      action: {"action", {:enum, hold_completion: "hold_completion"}},
      amount: {"amount", :number},
      order_id: {"order_id", {:string, :generic}},
      public_key: {"public_key", {:string, :generic}},
      rro_info: {"rro_info", {LiqPayAPI.InternetAcquiring.TwoStep.Complete.Request.RROInfo, :t}},
      version: {"version", {:enum, [3]}}
    ]
  end
end
