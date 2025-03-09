defmodule LiqPayAPI.InternetAcquiring.Refund.Response do
  @moduledoc """
  Provides struct and type for a InternetAcquiring.Refund.Response
  """

  @behaviour OpenAPIClient.Schema

  @type t :: %__MODULE__{
          action: :hold | :pay | :paydonate | :subscribe | String.t() | nil,
          payment_id: integer | nil,
          result: :error | :ok | String.t() | nil,
          status: :error | :failure | :reversed | :success | String.t() | nil,
          wait_amount: boolean | nil
        }
  @type types :: :t

  defstruct [:action, :payment_id, :result, :status, :wait_amount]

  @doc false
  @impl OpenAPIClient.Schema
  @spec __fields__(types()) :: keyword(OpenAPIClient.Schema.field_type())
  def __fields__(:t) do
    [
      action:
        {"action",
         {:enum,
          [
            {:hold, "hold"},
            {:pay, "pay"},
            {:paydonate, "paydonate"},
            {:subscribe, "subscribe"},
            :not_strict
          ]}},
      payment_id: {"payment_id", :integer},
      result: {"result", {:enum, [{:error, "error"}, {:ok, "ok"}, :not_strict]}},
      status:
        {"status",
         {:enum,
          [
            {:error, "error"},
            {:failure, "failure"},
            {:reversed, "reversed"},
            {:success, "success"},
            :not_strict
          ]}},
      wait_amount: {"wait_amount", :boolean}
    ]
  end
end
