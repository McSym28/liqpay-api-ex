defmodule LiqPayAPI.P2PdebitTransfer.Request do
  @moduledoc """
  Provides struct and type for a P2PdebitTransfer.Request
  """

  @behaviour OpenAPIClient.Schema

  @type t :: %__MODULE__{
          action: :p2pdebit,
          amount: number,
          card: String.t(),
          card_cvv: String.t(),
          card_exp_month: String.t(),
          card_exp_year: String.t(),
          card_token: String.t(),
          currency: :eur | :uah | :usd,
          description: String.t(),
          language: :en | :uk | nil,
          mpi_cres: String.t(),
          mpi_eci: 5 | 6 | 7,
          order_id: String.t(),
          phone: String.t(),
          prepare: true | nil,
          public_key: String.t(),
          recurringbytoken: true | nil,
          result_url: String.t() | nil,
          sandbox: true,
          sender: LiqPayAPI.P2PdebitTransfer.Request.Sender.t() | nil,
          server_url: String.t() | nil,
          version: 3
        }
  @type types :: :t

  @enforce_keys [
    :amount,
    :card,
    :card_cvv,
    :card_exp_month,
    :card_exp_year,
    :card_token,
    :currency,
    :description,
    :mpi_cres,
    :mpi_eci,
    :order_id,
    :phone
  ]
  defstruct [
    :amount,
    :card,
    :card_cvv,
    :card_exp_month,
    :card_exp_year,
    :card_token,
    :currency,
    :description,
    :language,
    :mpi_cres,
    :mpi_eci,
    :order_id,
    :phone,
    :prepare,
    :public_key,
    :recurringbytoken,
    :result_url,
    :sender,
    :server_url,
    action: :p2pdebit,
    sandbox: true,
    version: 3
  ]

  @doc false
  @impl OpenAPIClient.Schema
  @spec __fields__(types()) :: keyword(OpenAPIClient.Schema.field_type())
  def __fields__(:t) do
    [
      action: {"action", {:enum, p2pdebit: "p2pdebit"}},
      amount: {"amount", :number},
      card: {"card", {:string, :generic}},
      card_cvv: {"card_cvv", {:string, :generic}},
      card_exp_month: {"card_exp_month", {:string, :generic}},
      card_exp_year: {"card_exp_year", {:string, :generic}},
      card_token: {"card_token", {:string, :generic}},
      currency: {"currency", {:enum, eur: "EUR", uah: "UAH", usd: "USD"}},
      description: {"description", {:string, :generic}},
      language: {"language", {:enum, en: "en", uk: "uk"}},
      mpi_cres: {"mpi_cres", {:string, :generic}},
      mpi_eci: {"mpi_eci", {:enum, [5, 6, 7]}},
      order_id: {"order_id", {:string, :generic}},
      phone: {"phone", {:string, :generic}},
      prepare: {"prepare", {:enum, true: "1"}},
      public_key:
        {"public_key", {:string, :generic},
         fn -> Application.get_env(:liqpay_api_ex, :public_key) end},
      recurringbytoken: {"recurringbytoken", {:enum, true: "1"}},
      result_url: {"result_url", {:string, :uri}},
      sandbox: {"sandbox", {:enum, true: "1"}},
      sender: {"sender", {LiqPayAPI.P2PdebitTransfer.Request.Sender, :t}},
      server_url: {"server_url", {:string, :uri}},
      version: {"version", {:enum, [3]}}
    ]
  end
end
