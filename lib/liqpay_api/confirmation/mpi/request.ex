defmodule LiqPayAPI.Confirmation.MPI.Request do
  @moduledoc """
  Provides struct and type for a Confirmation.MPI.Request
  """

  @behaviour OpenAPIClient.Schema

  @type t :: %__MODULE__{
          action: :mpi,
          action_payment: :auth | :hold | :p2p | :p2pdebit | :pay | :paydonate | :subscribe | nil,
          amount: number,
          card: String.t(),
          card_cvv: String.t(),
          card_exp_month: String.t(),
          card_exp_year: String.t(),
          currency: :eur | :uah | :usd,
          description: String.t(),
          email: String.t() | nil,
          ip: String.t(),
          language: :en | :uk | nil,
          order_id: String.t(),
          phone: String.t() | nil,
          public_key: String.t(),
          sender_first_name: String.t() | nil,
          sender_last_name: String.t() | nil,
          three_ds_info: LiqPayAPI.Confirmation.MPI.Request.ThreeDSInfo.t(),
          version: 3
        }
  @type types :: :t

  @enforce_keys [
    :amount,
    :card,
    :card_cvv,
    :card_exp_month,
    :card_exp_year,
    :currency,
    :description,
    :ip,
    :order_id,
    :three_ds_info
  ]
  defstruct [
    :action_payment,
    :amount,
    :card,
    :card_cvv,
    :card_exp_month,
    :card_exp_year,
    :currency,
    :description,
    :email,
    :ip,
    :language,
    :order_id,
    :phone,
    :public_key,
    :sender_first_name,
    :sender_last_name,
    :three_ds_info,
    action: :mpi,
    version: 3
  ]

  @doc false
  @impl OpenAPIClient.Schema
  @spec __fields__(types()) :: keyword(OpenAPIClient.Schema.schema_type())
  def __fields__(:t) do
    [
      action: {"action", {:enum, mpi: "mpi"}},
      action_payment:
        {"action_payment",
         {:enum,
          auth: "auth",
          hold: "hold",
          p2p: "p2p",
          p2pdebit: "p2pdebit",
          pay: "pay",
          paydonate: "paydonate",
          subscribe: "subscribe"}},
      amount: {"amount", :number},
      card: {"card", {:string, :generic}},
      card_cvv: {"card_cvv", {:string, :generic}},
      card_exp_month: {"card_exp_month", {:string, :generic}},
      card_exp_year: {"card_exp_year", {:string, :generic}},
      currency: {"currency", {:enum, eur: "EUR", uah: "UAH", usd: "USD"}},
      description: {"description", {:string, :generic}},
      email: {"email", {:string, :generic}},
      ip: {"ip", {:string, :generic}},
      language: {"language", {:enum, en: "en", uk: "uk"}},
      order_id: {"order_id", {:string, :generic}},
      phone: {"phone", {:string, :generic}},
      public_key: {"public_key", {:string, :generic}},
      sender_first_name: {"sender_first_name", {:string, :generic}},
      sender_last_name: {"sender_last_name", {:string, :generic}},
      three_ds_info: {"threeDSInfo", {LiqPayAPI.Confirmation.MPI.Request.ThreeDSInfo, :t}},
      version: {"version", {:enum, [3]}}
    ]
  end
end
