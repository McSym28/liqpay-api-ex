defmodule LiqPayAPI.Tokens.Obtain.Response do
  @moduledoc """
  Provides struct and type for a Tokens.Obtain.Response
  """

  @behaviour OpenAPIClient.Schema

  @type t :: %__MODULE__{
          card_token: String.t() | nil,
          card_token_info: LiqPayAPI.Tokens.Obtain.Response.CardTokenInfo.t() | nil,
          result: :error | :ok | String.t() | nil,
          status: String.t() | nil
        }
  @type types :: :t

  defstruct [:card_token, :card_token_info, :result, :status]

  @doc false
  @impl OpenAPIClient.Schema
  @spec __fields__(types()) :: keyword(OpenAPIClient.Schema.schema_type())
  def __fields__(:t) do
    [
      card_token: {"card_token", {:string, :generic}},
      card_token_info: {"card_token_info", {LiqPayAPI.Tokens.Obtain.Response.CardTokenInfo, :t}},
      result: {"result", {:enum, [{:error, "error"}, {:ok, "ok"}, :not_strict]}},
      status: {"status", {:string, :generic}}
    ]
  end
end
