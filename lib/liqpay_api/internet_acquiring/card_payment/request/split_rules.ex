defmodule LiqPayAPI.InternetAcquiring.CardPayment.Request.SplitRules do
  @moduledoc """
  Provides struct and type for a InternetAcquiring.CardPayment.Request.SplitRules
  """

  @behaviour OpenAPIClient.Schema

  @type t :: %__MODULE__{
          amount: number,
          commission_payer: :receiver | :sender | nil,
          description: String.t() | nil,
          public_key: String.t() | nil,
          rro_info: LiqPayAPI.InternetAcquiring.CardPayment.Request.SplitRulesRroInfo.t() | nil,
          server_url: String.t() | nil
        }
  @type types :: :t

  @enforce_keys [:amount]
  defstruct [:amount, :commission_payer, :description, :public_key, :rro_info, :server_url]

  @doc false
  @impl OpenAPIClient.Schema
  @spec __fields__(types()) :: keyword(OpenAPIClient.Schema.field_type())
  def __fields__(:t) do
    [
      amount: {"amount", :number},
      commission_payer: {"commission_payer", {:enum, receiver: "receiver", sender: "sender"}},
      description: {"description", {:string, :generic}},
      public_key: {"public_key", {:string, :generic}},
      rro_info:
        {"rro_info", {LiqPayAPI.InternetAcquiring.CardPayment.Request.SplitRulesRroInfo, :t}},
      server_url: {"server_url", {:string, :uri}}
    ]
  end
end
