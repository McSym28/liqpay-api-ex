defmodule LiqPayAPI.TransferringToCard.Transfer.Request do
  @moduledoc """
  Provides struct and type for a TransferringToCard.Transfer.Request
  """

  @behaviour OpenAPIClient.Schema

  @type t :: %__MODULE__{
          action: :p2pcredit,
          amount: number,
          currency: :eur | :uah | :usd,
          customer: String.t() | nil,
          description: String.t(),
          info: String.t() | nil,
          ip: String.t(),
          language: :en | :uk | nil,
          order_id: String.t(),
          public_key: String.t(),
          receiver_account:
            LiqPayAPI.TransferringToCard.Transfer.Request.ReceiverAccount.t() | nil,
          receiver_card: String.t() | nil,
          receiver_card_token: String.t() | nil,
          receiver_first_name: String.t() | nil,
          receiver_last_name: String.t() | nil,
          sender: LiqPayAPI.TransferringToCard.Transfer.Request.Sender.t() | nil,
          server_url: String.t() | nil,
          taxed: :"income is not subject to tax" | nil,
          version: 3
        }
  @type types :: :t

  @enforce_keys [:amount, :currency, :description, :ip, :order_id]
  defstruct [
    :amount,
    :currency,
    :customer,
    :description,
    :info,
    :ip,
    :language,
    :order_id,
    :public_key,
    :receiver_account,
    :receiver_card,
    :receiver_card_token,
    :receiver_first_name,
    :receiver_last_name,
    :sender,
    :server_url,
    :taxed,
    action: :p2pcredit,
    version: 3
  ]

  @doc false
  @impl OpenAPIClient.Schema
  @spec __fields__(types()) :: keyword(OpenAPIClient.Schema.schema_type())
  def __fields__(:t) do
    [
      action: {"action", {:enum, p2pcredit: "p2pcredit"}},
      amount: {"amount", :number},
      currency: {"currency", {:enum, eur: "EUR", uah: "UAH", usd: "USD"}},
      customer: {"customer", {:string, :generic}},
      description: {"description", {:string, :generic}},
      info: {"info", {:string, :generic}},
      ip: {"ip", {:string, :generic}},
      language: {"language", {:enum, en: "en", uk: "uk"}},
      order_id: {"order_id", {:string, :generic}},
      public_key: {"public_key", {:string, :generic}},
      receiver_account:
        {"receiver_account", {LiqPayAPI.TransferringToCard.Transfer.Request.ReceiverAccount, :t}},
      receiver_card: {"receiver_card", {:string, :generic}},
      receiver_card_token: {"receiver_card_token", {:string, :generic}},
      receiver_first_name: {"receiver_first_name", {:string, :generic}},
      receiver_last_name: {"receiver_last_name", {:string, :generic}},
      sender: {"sender", {LiqPayAPI.TransferringToCard.Transfer.Request.Sender, :t}},
      server_url: {"server_url", {:string, :uri}},
      taxed: {"taxed", {:enum, "income is not subject to tax": "Income is not subject to tax"}},
      version: {"version", {:enum, [3]}}
    ]
  end
end
