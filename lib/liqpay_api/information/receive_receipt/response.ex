defmodule LiqPayAPI.Information.ReceiveReceipt.Response do
  @moduledoc """
  Provides struct and type for a Information.ReceiveReceipt.Response
  """

  @behaviour OpenAPIClient.Schema

  @type t :: %__MODULE__{result: :error | :ok | String.t() | nil}
  @type types :: :t

  defstruct [:result]

  @doc false
  @impl OpenAPIClient.Schema
  @spec __fields__(types()) :: keyword(OpenAPIClient.Schema.schema_type())
  def __fields__(:t) do
    [result: {"result", {:enum, [{:error, "error"}, {:ok, "ok"}, :not_strict]}}]
  end
end
