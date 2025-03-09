defmodule LiqPayAPI.InternetAcquiring.GPay.DecryptedToken.Request do
  @moduledoc """
  Provides struct and type for a InternetAcquiring.GPay.DecryptedToken.Request
  """

  @behaviour OpenAPIClient.Schema

  @type t :: %__MODULE__{
          action: :pay,
          amount: number,
          currency: :eur | :uah | :usd,
          description: String.t(),
          gpay_token: String.t(),
          language: :en | :uk | nil,
          order_id: String.t(),
          paytype: :gpay,
          public_key: String.t(),
          result_url: String.t() | nil,
          server_url: String.t() | nil,
          split_tickets_only: boolean | nil,
          version: 3
        }
  @type types :: :t

  @enforce_keys [:amount, :currency, :description, :gpay_token, :order_id]
  defstruct [
    :amount,
    :currency,
    :description,
    :gpay_token,
    :language,
    :order_id,
    :public_key,
    :result_url,
    :server_url,
    :split_tickets_only,
    action: :pay,
    paytype: :gpay,
    version: 3
  ]

  @doc false
  @impl OpenAPIClient.Schema
  @spec __fields__(types()) :: keyword(OpenAPIClient.Schema.field_type())
  def __fields__(:t) do
    [
      action: {"action", {:enum, pay: "pay"}},
      amount: {"amount", :number},
      currency: {"currency", {:enum, eur: "EUR", uah: "UAH", usd: "USD"}},
      description: {"description", {:string, :generic}},
      gpay_token: {"gpay_token", {:string, :generic}},
      language: {"language", {:enum, en: "en", uk: "uk"}},
      order_id: {"order_id", {:string, :generic}},
      paytype: {"paytype", {:enum, gpay: "gpay"}},
      public_key:
        {"public_key", {:string, :generic},
         fn -> Application.get_env(:liqpay_api_ex, :public_key) end},
      result_url: {"result_url", {:string, :uri}},
      server_url: {"server_url", {:string, :uri}},
      split_tickets_only: {"split_tickets_only", :boolean},
      version: {"version", {:enum, [3]}}
    ]
  end
end
