defmodule LiqPayAPI.InternetAcquiring.Invoice.Issue.Response do
  @moduledoc """
  Provides struct and type for a InternetAcquiring.Invoice.Issue.Response
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
          amount: number | nil,
          currency: String.t() | nil,
          description: String.t() | nil,
          href: String.t() | nil,
          id: integer | nil,
          order_id: String.t() | nil,
          receiver_type: String.t() | nil,
          receiver_value: String.t() | nil,
          result: :error | :ok | String.t() | nil,
          status: :error | :failure | :invoice_wait | :success | String.t() | nil,
          token: String.t() | nil
        }
  @type types :: :t

  defstruct [
    :action,
    :amount,
    :currency,
    :description,
    :href,
    :id,
    :order_id,
    :receiver_type,
    :receiver_value,
    :result,
    :status,
    :token
  ]

  @doc false
  @impl OpenAPIClient.Schema
  @spec __fields__(types()) :: keyword(OpenAPIClient.Schema.field_type())
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
      amount: {"amount", :number},
      currency: {"currency", {:string, :generic}},
      description: {"description", {:string, :generic}},
      href: {"href", {:string, :generic}},
      id: {"id", :integer},
      order_id: {"order_id", {:string, :generic}},
      receiver_type: {"receiver_type", {:string, :generic}},
      receiver_value: {"receiver_value", {:string, :generic}},
      result: {"result", {:enum, [{:error, "error"}, {:ok, "ok"}, :not_strict]}},
      status:
        {"status",
         {:enum,
          [
            {:error, "error"},
            {:failure, "failure"},
            {:invoice_wait, "invoice_wait"},
            {:success, "success"},
            :not_strict
          ]}},
      token: {"token", {:string, :generic}}
    ]
  end
end
