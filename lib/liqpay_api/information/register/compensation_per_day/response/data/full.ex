defmodule LiqPayAPI.Information.Register.CompensationPerDay.Response.Data.Full do
  @moduledoc """
  Provides struct and type for a Information.Register.CompensationPerDay.Response.Data.Full
  """

  @behaviour OpenAPIClient.Schema

  @type t :: %__MODULE__{
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
          authcode_debit: String.t() | nil,
          bonus_type: :bonusplus | :discount_club | :personal | :promo | String.t() | nil,
          channel: :api | :checkout | :checkoutjs | String.t() | nil,
          create_date: DateTime.t() | nil,
          customer: String.t() | nil,
          description: String.t() | nil,
          end_date: DateTime.t() | nil,
          id: number | nil,
          ip: String.t() | nil,
          liqpay_order_id: String.t() | nil,
          order_id: String.t() | nil,
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
          sender_card: String.t() | nil,
          sender_card_bank: String.t() | nil,
          sender_card_country: String.t() | nil,
          sender_card_product_type: String.t() | nil,
          sender_card_type: String.t() | nil,
          sender_email: String.t() | nil,
          sender_first_name: String.t() | nil,
          sender_last_name: String.t() | nil,
          sender_phone: String.t() | nil,
          trans_amount: number | nil,
          trans_bonus: number | nil,
          trans_currency: String.t() | nil,
          trans_fee_credit: number | nil,
          trans_fee_debit: number | nil,
          trans_total: number | nil,
          trans_type: :hold_compl | :purchase | :refund | :reverse | String.t() | nil
        }
  @type types :: :t

  defstruct [
    :action,
    :authcode_debit,
    :bonus_type,
    :channel,
    :create_date,
    :customer,
    :description,
    :end_date,
    :id,
    :ip,
    :liqpay_order_id,
    :order_id,
    :paytype,
    :sender_card,
    :sender_card_bank,
    :sender_card_country,
    :sender_card_product_type,
    :sender_card_type,
    :sender_email,
    :sender_first_name,
    :sender_last_name,
    :sender_phone,
    :trans_amount,
    :trans_bonus,
    :trans_currency,
    :trans_fee_credit,
    :trans_fee_debit,
    :trans_total,
    :trans_type
  ]

  @doc false
  @impl OpenAPIClient.Schema
  @spec __fields__(types()) :: keyword(OpenAPIClient.Schema.schema_type())
  def __fields__(:t) do
    [
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
      authcode_debit: {"authcode_debit", {:string, :generic}},
      bonus_type:
        {"bonus_type",
         {:enum,
          [
            {:bonusplus, "bonusplus"},
            {:discount_club, "discount_club"},
            {:personal, "personal"},
            {:promo, "promo"},
            :not_strict
          ]}},
      channel:
        {"channel",
         {:enum,
          [{:api, "api"}, {:checkout, "checkout"}, {:checkoutjs, "checkoutjs"}, :not_strict]}},
      create_date: {"create_date", {:string, "date-time-liqpay"}},
      customer: {"customer", {:string, :generic}},
      description: {"description", {:string, :generic}},
      end_date: {"end_date", {:string, "date-time-liqpay"}},
      id: {"id", :number},
      ip: {"ip", {:string, :generic}},
      liqpay_order_id: {"liqpay_order_id", {:string, :generic}},
      order_id: {"order_id", {:string, :generic}},
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
      sender_card: {"sender_card", {:string, :generic}},
      sender_card_bank: {"sender_card_bank", {:string, :generic}},
      sender_card_country: {"sender_card_country", {:string, :generic}},
      sender_card_product_type: {"sender_card_product_type", {:string, :generic}},
      sender_card_type: {"sender_card_type", {:string, :generic}},
      sender_email: {"sender_email", {:string, :generic}},
      sender_first_name: {"sender_first_name", {:string, :generic}},
      sender_last_name: {"sender_last_name", {:string, :generic}},
      sender_phone: {"sender_phone", {:string, :generic}},
      trans_amount: {"trans_amount", :number},
      trans_bonus: {"trans_bonus", :number},
      trans_currency: {"trans_currency", {:string, :generic}},
      trans_fee_credit: {"trans_fee_credit", :number},
      trans_fee_debit: {"trans_fee_debit", :number},
      trans_total: {"trans_total", :number},
      trans_type:
        {"trans_type",
         {:enum,
          [
            {:hold_compl, "hold_compl"},
            {:purchase, "purchase"},
            {:refund, "refund"},
            {:reverse, "reverse"},
            :not_strict
          ]}}
    ]
  end
end
