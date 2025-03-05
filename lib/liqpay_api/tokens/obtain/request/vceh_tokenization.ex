defmodule LiqPayAPI.Tokens.Obtain.Request.VCEHTokenization do
  @moduledoc """
  Provides struct and type for a Tokens.Obtain.Request.VCEHTokenization
  """

  @behaviour OpenAPIClient.Schema

  @type t :: %__MODULE__{customer: String.t(), pushdata: String.t()}
  @type types :: :t

  @enforce_keys [:customer, :pushdata]
  defstruct [:customer, :pushdata]

  @doc false
  @impl OpenAPIClient.Schema
  @spec __fields__(types()) :: keyword(OpenAPIClient.Schema.schema_type())
  def __fields__(:t) do
    [customer: {"customer", {:string, :generic}}, pushdata: {"pushdata", {:string, :generic}}]
  end
end
