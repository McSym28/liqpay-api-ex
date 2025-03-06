defmodule LiqPayAPI.InternetAcquiring.Invoice.Cancel.Response do
  @moduledoc """
  Provides struct and type for a InternetAcquiring.Invoice.Cancel.Response
  """

  @behaviour OpenAPIClient.Schema

  @type t :: %__MODULE__{invoice_id: number | nil, result: :error | :ok | String.t() | nil}
  @type types :: :t

  defstruct [:invoice_id, :result]

  @doc false
  @impl OpenAPIClient.Schema
  @spec __fields__(types()) :: keyword(OpenAPIClient.Schema.field_type())
  def __fields__(:t) do
    [
      invoice_id: {"invoice_id", :number},
      result: {"result", {:enum, [{:error, "error"}, {:ok, "ok"}, :not_strict]}}
    ]
  end
end
