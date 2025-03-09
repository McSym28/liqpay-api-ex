defmodule LiqPayAPI.Callbacks.CallbackRequest do
  @moduledoc """
  Provides struct and type for a Callbacks.CallbackRequest
  """

  @behaviour OpenAPIClient.Schema

  @type t :: %__MODULE__{
          acq_id: integer | nil,
          action: :hold | :pay | :paysplit | :regular | :subscribe | String.t() | nil,
          agent_commission: number | nil,
          amount: number | nil,
          amount_bonus: number | nil,
          amount_credit: number | nil,
          amount_debit: number | nil,
          authcode_credit: String.t() | nil,
          authcode_debit: String.t() | nil,
          card_token: String.t() | nil,
          commission_credit: number | nil,
          commission_debit: number | nil,
          completion_date: DateTime.t() | nil,
          create_date: DateTime.t() | nil,
          currency: String.t() | nil,
          currency_credit: String.t() | nil,
          currency_debit: String.t() | nil,
          customer: String.t() | nil,
          description: String.t() | nil,
          end_date: DateTime.t() | nil,
          err_code: String.t() | nil,
          err_description: String.t() | nil,
          err_erc: String.t() | nil,
          info: String.t() | nil,
          ip: String.t() | nil,
          is_3ds: boolean | nil,
          liqpay_order_id: String.t() | nil,
          mpi_eci: 5 | 6 | 7 | integer | nil,
          order_id: String.t() | nil,
          payment_id: integer | nil,
          paytype:
            :card
            | :cash
            | :invoice
            | :masterpass
            | :moment_part
            | :privat24
            | :qr
            | String.t()
            | nil,
          product_category: String.t() | nil,
          product_description: String.t() | nil,
          product_name: String.t() | nil,
          product_url: String.t() | nil,
          public_key: String.t() | nil,
          receiver_commission: number | nil,
          redirect_to: String.t() | nil,
          refund_amount: number | nil,
          refund_date_last: DateTime.t() | nil,
          rrn_credit: String.t() | nil,
          rrn_debit: String.t() | nil,
          sender_bonus: number | nil,
          sender_card_bank: String.t() | nil,
          sender_card_country: integer | nil,
          sender_card_mask2: String.t() | nil,
          sender_card_type: String.t() | nil,
          sender_commission: number | nil,
          sender_first_name: String.t() | nil,
          sender_last_name: String.t() | nil,
          sender_phone: String.t() | nil,
          status:
            :"3ds_verify"
            | :captcha_verify
            | :cash_wait
            | :cvv_verify
            | :error
            | :failure
            | :hold_wait
            | :invoice_wait
            | :ivr_verify
            | :password_verify
            | :phone_verify
            | :pin_verify
            | :prepared
            | :processing
            | :receiver_verify
            | :reversed
            | :sender_verify
            | :senderapp_verify
            | :subscribed
            | :success
            | :unsubscribed
            | :wait_accept
            | :wait_card
            | :wait_compensation
            | :wait_lc
            | :wait_qr
            | :wait_reserve
            | :wait_secure
            | :wait_sender
            | String.t()
            | nil,
          token: String.t() | nil,
          type: String.t() | nil,
          verifycode: true | boolean() | nil,
          version: 3 | integer | nil,
          wait_reserve_status: true | boolean | nil
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
    :authcode_credit,
    :authcode_debit,
    :card_token,
    :commission_credit,
    :commission_debit,
    :completion_date,
    :create_date,
    :currency,
    :currency_credit,
    :currency_debit,
    :customer,
    :description,
    :end_date,
    :err_code,
    :err_description,
    :err_erc,
    :info,
    :ip,
    :is_3ds,
    :liqpay_order_id,
    :mpi_eci,
    :order_id,
    :payment_id,
    :paytype,
    :product_category,
    :product_description,
    :product_name,
    :product_url,
    :public_key,
    :receiver_commission,
    :redirect_to,
    :refund_amount,
    :refund_date_last,
    :rrn_credit,
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
    :token,
    :type,
    :verifycode,
    :version,
    :wait_reserve_status
  ]

  @doc false
  @impl OpenAPIClient.Schema
  @spec __fields__(types()) :: keyword(OpenAPIClient.Schema.field_type())
  def __fields__(:t) do
    [
      acq_id: {"acq_id", :integer},
      action:
        {"action",
         {:enum,
          [
            {:hold, "hold"},
            {:pay, "pay"},
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
      authcode_credit: {"authcode_credit", {:string, :generic}},
      authcode_debit: {"authcode_debit", {:string, :generic}},
      card_token: {"card_token", {:string, :generic}},
      commission_credit: {"commission_credit", :number},
      commission_debit: {"commission_debit", :number},
      completion_date: {"completion_date", {:integer, "timestamp-ms"}},
      create_date: {"create_date", {:integer, "timestamp-ms"}},
      currency: {"currency", {:string, :generic}},
      currency_credit: {"currency_credit", {:string, :generic}},
      currency_debit: {"currency_debit", {:string, :generic}},
      customer: {"customer", {:string, :generic}},
      description: {"description", {:string, :generic}},
      end_date: {"end_date", {:integer, "timestamp-ms"}},
      err_code: {"err_code", {:string, :generic}},
      err_description: {"err_description", {:string, :generic}},
      err_erc: {"err_erc", {:string, :generic}},
      info: {"info", {:string, :generic}},
      ip: {"ip", {:string, :generic}},
      is_3ds: {"is_3ds", :boolean},
      liqpay_order_id: {"liqpay_order_id", {:string, :generic}},
      mpi_eci: {"mpi_eci", {:enum, [5, 6, 7, :not_strict]}},
      order_id: {"order_id", {:string, :generic}},
      payment_id: {"payment_id", :integer},
      paytype:
        {"paytype",
         {:enum,
          [
            {:card, "card"},
            {:cash, "cash"},
            {:invoice, "invoice"},
            {:masterpass, "masterpass"},
            {:moment_part, "moment_part"},
            {:privat24, "privat24"},
            {:qr, "qr"},
            :not_strict
          ]}},
      product_category: {"product_category", {:string, :generic}},
      product_description: {"product_description", {:string, :generic}},
      product_name: {"product_name", {:string, :generic}},
      product_url: {"product_url", {:string, :uri}},
      public_key: {"public_key", {:string, :generic}},
      receiver_commission: {"receiver_commission", :number},
      redirect_to: {"redirect_to", {:string, :generic}},
      refund_amount: {"refund_amount", :number},
      refund_date_last: {"refund_date_last", {:integer, "timestamp-ms"}},
      rrn_credit: {"rrn_credit", {:string, :generic}},
      rrn_debit: {"rrn_debit", {:string, :generic}},
      sender_bonus: {"sender_bonus", :number},
      sender_card_bank: {"sender_card_bank", {:string, :generic}},
      sender_card_country: {"sender_card_country", :integer},
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
            {:captcha_verify, "captcha_verify"},
            {:cash_wait, "cash_wait"},
            {:cvv_verify, "cvv_verify"},
            {:error, "error"},
            {:failure, "failure"},
            {:hold_wait, "hold_wait"},
            {:invoice_wait, "invoice_wait"},
            {:ivr_verify, "ivr_verify"},
            {:password_verify, "password_verify"},
            {:phone_verify, "phone_verify"},
            {:pin_verify, "pin_verify"},
            {:prepared, "prepared"},
            {:processing, "processing"},
            {:receiver_verify, "receiver_verify"},
            {:reversed, "reversed"},
            {:sender_verify, "sender_verify"},
            {:senderapp_verify, "senderapp_verify"},
            {:subscribed, "subscribed"},
            {:success, "success"},
            {:unsubscribed, "unsubscribed"},
            {:wait_accept, "wait_accept"},
            {:wait_card, "wait_card"},
            {:wait_compensation, "wait_compensation"},
            {:wait_lc, "wait_lc"},
            {:wait_qr, "wait_qr"},
            {:wait_reserve, "wait_reserve"},
            {:wait_secure, "wait_secure"},
            {:wait_sender, "wait_sender"},
            :not_strict
          ]}},
      token: {"token", {:string, :generic}},
      type: {"type", {:string, :generic}},
      verifycode: {"verifycode", {:enum, [{true, "Y"}, :not_strict]}},
      version: {"version", {:enum, [3, :not_strict]}},
      wait_reserve_status: {"wait_reserve_status", {:enum, [true, :not_strict]}}
    ]
  end
end
