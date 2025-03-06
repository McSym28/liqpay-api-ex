defmodule LiqPayAPI.Tokens.ChangeStatus.Response do
  @moduledoc """
  Provides struct and type for a Tokens.ChangeStatus.Response
  """

  @behaviour OpenAPIClient.Schema

  @type t :: %__MODULE__{
          card_token: String.t() | nil,
          card_token_info: LiqPayAPI.Tokens.ChangeStatus.Response.CardTokenInfo.t() | nil,
          status: String.t() | nil
        }
  @type types :: :t

  defstruct [:card_token, :card_token_info, :status]

  @doc false
  @impl OpenAPIClient.Schema
  @spec __fields__(types()) :: keyword(OpenAPIClient.Schema.field_type())
  def __fields__(:t) do
    [
      card_token: {"card_token", {:string, :generic}},
      card_token_info:
        {"card_token_info", {LiqPayAPI.Tokens.ChangeStatus.Response.CardTokenInfo, :t}},
      status: {"status", {:string, :generic}}
    ]
  end
end
