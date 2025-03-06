defmodule LiqPayAPI.InternetAcquiring.TwoStep.Block.Response do
  @moduledoc """
  Provides struct and type for a InternetAcquiring.TwoStep.Block.Response
  """

  @behaviour OpenAPIClient.Schema

  @type t :: %__MODULE__{
          acq_id: number | nil,
          action:
            :auth
            | :hold
            | :pay
            | :paydonate
            | :paysplit
            | :regular
            | :subscribe
            | String.t()
            | nil,
          agent_commission: number | nil,
          amount: number | nil,
          amount_bonus: number | nil,
          amount_credit: number | nil,
          amount_debit: number | nil,
          authcode_debit: String.t() | nil,
          card_token: String.t() | nil,
          commission_credit: number | nil,
          commission_debit: number | nil,
          create_date: DateTime.t() | nil,
          currency: String.t() | nil,
          currency_credit: String.t() | nil,
          currency_debit: String.t() | nil,
          description: String.t() | nil,
          end_date: DateTime.t() | nil,
          ip: String.t() | nil,
          is_3ds: boolean | nil,
          liqpay_order_id: String.t() | nil,
          mpi_eci: 5 | 6 | 7 | integer | nil,
          order_id: String.t() | nil,
          payment_id: number | nil,
          paytype: :card | :cash | :invoice | :moment_part | :privat24 | :qr | String.t() | nil,
          public_key: String.t() | nil,
          receiver_commission: number | nil,
          rrn_debit: String.t() | nil,
          sender_bonus: number | nil,
          sender_card_bank: String.t() | nil,
          sender_card_country: String.t() | nil,
          sender_card_mask2: String.t() | nil,
          sender_card_type: String.t() | nil,
          sender_commission: number | nil,
          sender_first_name: String.t() | nil,
          sender_last_name: String.t() | nil,
          sender_phone: String.t() | nil,
          status:
            :"3ds_verify"
            | :cvv_verify
            | :error
            | :failure
            | :hold_wait
            | :receiver_verify
            | :reversed
            | :sender_verify
            | :success
            | :wait_accept
            | :wait_secure
            | String.t()
            | nil,
          transaction_id: number | nil,
          type: String.t() | nil,
          version: 3 | integer | nil
        }
  @type types :: :t

  defstruct [
    :acq_id,
    :action,
    :agent_commission,
    :amount,
    :amount_bonus,
    :amount_credit,
    :amount_debit,
    :authcode_debit,
    :card_token,
    :commission_credit,
    :commission_debit,
    :create_date,
    :currency,
    :currency_credit,
    :currency_debit,
    :description,
    :end_date,
    :ip,
    :is_3ds,
    :liqpay_order_id,
    :mpi_eci,
    :order_id,
    :payment_id,
    :paytype,
    :public_key,
    :receiver_commission,
    :rrn_debit,
    :sender_bonus,
    :sender_card_bank,
    :sender_card_country,
    :sender_card_mask2,
    :sender_card_type,
    :sender_commission,
    :sender_first_name,
    :sender_last_name,
    :sender_phone,
    :status,
    :transaction_id,
    :type,
    :version
  ]

  @doc false
  @impl OpenAPIClient.Schema
  @spec __fields__(types()) :: keyword(OpenAPIClient.Schema.field_type())
  def __fields__(:t) do
    [
      acq_id: {"acq_id", :number},
      action:
        {"action",
         {:enum,
          [
            {:auth, "auth"},
            {:hold, "hold"},
            {:pay, "pay"},
            {:paydonate, "paydonate"},
            {:paysplit, "paysplit"},
            {:regular, "regular"},
            {:subscribe, "subscribe"},
            :not_strict
          ]}},
      agent_commission: {"agent_commission", :number},
      amount: {"amount", :number},
      amount_bonus: {"amount_bonus", :number},
      amount_credit: {"amount_credit", :number},
      amount_debit: {"amount_debit", :number},
      authcode_debit: {"authcode_debit", {:string, :generic}},
      card_token: {"card_token", {:string, :generic}},
      commission_credit: {"commission_credit", :number},
      commission_debit: {"commission_debit", :number},
      create_date: {"create_date", {:integer, "timestamp-ms"}},
      currency: {"currency", {:string, :generic}},
      currency_credit: {"currency_credit", {:string, :generic}},
      currency_debit: {"currency_debit", {:string, :generic}},
      description: {"description", {:string, :generic}},
      end_date: {"end_date", {:integer, "timestamp-ms"}},
      ip: {"ip", {:string, :generic}},
      is_3ds: {"is_3ds", :boolean},
      liqpay_order_id: {"liqpay_order_id", {:string, :generic}},
      mpi_eci: {"mpi_eci", {:enum, [5, 6, 7, :not_strict]}},
      order_id: {"order_id", {:string, :generic}},
      payment_id: {"payment_id", :number},
      paytype:
        {"paytype",
         {:enum,
          [
            {:card, "card"},
            {:cash, "cash"},
            {:invoice, "invoice"},
            {:moment_part, "moment_part"},
            {:privat24, "privat24"},
            {:qr, "qr"},
            :not_strict
          ]}},
      public_key: {"public_key", {:string, :generic}},
      receiver_commission: {"receiver_commission", :number},
      rrn_debit: {"rrn_debit", {:string, :generic}},
      sender_bonus: {"sender_bonus", :number},
      sender_card_bank: {"sender_card_bank", {:string, :generic}},
      sender_card_country: {"sender_card_country", {:string, :generic}},
      sender_card_mask2: {"sender_card_mask2", {:string, :generic}},
      sender_card_type: {"sender_card_type", {:string, :generic}},
      sender_commission: {"sender_commission", :number},
      sender_first_name: {"sender_first_name", {:string, :generic}},
      sender_last_name: {"sender_last_name", {:string, :generic}},
      sender_phone: {"sender_phone", {:string, :generic}},
      status:
        {"status",
         {:enum,
          [
            {:"3ds_verify", "3ds_verify"},
            {:cvv_verify, "cvv_verify"},
            {:error, "error"},
            {:failure, "failure"},
            {:hold_wait, "hold_wait"},
            {:receiver_verify, "receiver_verify"},
            {:reversed, "reversed"},
            {:sender_verify, "sender_verify"},
            {:success, "success"},
            {:wait_accept, "wait_accept"},
            {:wait_secure, "wait_secure"},
            :not_strict
          ]}},
      transaction_id: {"transaction_id", :number},
      type: {"type", {:string, :generic}},
      version: {"version", {:enum, [3, :not_strict]}}
    ]
  end
end
