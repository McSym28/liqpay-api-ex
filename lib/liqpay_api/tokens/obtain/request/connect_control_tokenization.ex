defmodule LiqPayAPI.Tokens.Obtain.Request.ConnectControlTokenization do
  @moduledoc """
  Provides struct and type for a Tokens.Obtain.Request.ConnectControlTokenization
  """

  @behaviour OpenAPIClient.Schema

  @type t :: %__MODULE__{push_account_receipt: String.t()}
  @type types :: :t

  @enforce_keys [:push_account_receipt]
  defstruct [:push_account_receipt]

  @doc false
  @impl OpenAPIClient.Schema
  @spec __fields__(types()) :: keyword(OpenAPIClient.Schema.field_type())
  def __fields__(:t) do
    [push_account_receipt: {"pushAccountReceipt", {:string, :generic}}]
  end
end
