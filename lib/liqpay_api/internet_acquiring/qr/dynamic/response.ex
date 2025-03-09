defmodule LiqPayAPI.InternetAcquiring.QR.Dynamic.Response do
  @moduledoc """
  Provides struct and type for a InternetAcquiring.QR.Dynamic.Response
  """

  @behaviour OpenAPIClient.Schema

  @type t :: %__MODULE__{
          acq_id: integer | nil,
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
          payment_id: integer | nil,
          public_key: String.t() | nil,
          qr_code: String.t() | nil,
          receiver_commission: number | nil,
          result: :error | :ok | String.t() | nil,
          sender_bonus: number | nil,
          sender_commission: number | nil,
          status: :error | :failure | :success | :wait_qr | String.t() | nil,
          transaction_id: integer | nil,
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
    :public_key,
    :qr_code,
    :receiver_commission,
    :result,
    :sender_bonus,
    :sender_commission,
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
      acq_id: {"acq_id", :integer},
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
      payment_id: {"payment_id", :integer},
      public_key: {"public_key", {:string, :generic}},
      qr_code: {"qr_code", {:string, :generic}},
      receiver_commission: {"receiver_commission", :number},
      result: {"result", {:enum, [{:error, "error"}, {:ok, "ok"}, :not_strict]}},
      sender_bonus: {"sender_bonus", :number},
      sender_commission: {"sender_commission", :number},
      status:
        {"status",
         {:enum,
          [
            {:error, "error"},
            {:failure, "failure"},
            {:success, "success"},
            {:wait_qr, "wait_qr"},
            :not_strict
          ]}},
      transaction_id: {"transaction_id", :integer},
      type: {"type", {:string, :generic}},
      version: {"version", {:enum, [3, :not_strict]}}
    ]
  end
end
