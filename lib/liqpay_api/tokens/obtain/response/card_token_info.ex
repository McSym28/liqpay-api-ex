defmodule LiqPayAPI.Tokens.Obtain.Response.CardTokenInfo do
  @moduledoc """
  Provides struct and type for a Tokens.Obtain.Response.CardTokenInfo
  """

  @behaviour OpenAPIClient.Schema

  @type t :: %__MODULE__{
          decision:
            :approved
            | :cancelled
            | :declined
            | :error
            | :require_additional_authentication
            | String.t()
            | nil,
          status: :active | :deleted | :inactive | :suspended | String.t() | nil,
          token_exp_date: Date.t() | nil,
          token_ref: String.t() | nil,
          token_suffix: String.t() | nil
        }
  @type types :: :t

  defstruct [:decision, :status, :token_exp_date, :token_ref, :token_suffix]

  @doc false
  @impl OpenAPIClient.Schema
  @spec __fields__(types()) :: keyword(OpenAPIClient.Schema.field_type())
  def __fields__(:t) do
    [
      decision:
        {"decision",
         {:enum,
          [
            {:approved, "APPROVED"},
            {:cancelled, "CANCELLED"},
            {:declined, "DECLINED"},
            {:error, "ERROR"},
            {:require_additional_authentication, "REQUIRE_ADDITIONAL_AUTHENTICATION"},
            :not_strict
          ]}},
      status:
        {"status",
         {:enum,
          [
            {:active, "ACTIVE"},
            {:deleted, "DELETED"},
            {:inactive, "INACTIVE"},
            {:suspended, "SUSPENDED"},
            :not_strict
          ]}},
      token_exp_date: {"tokenExpDate", {:string, "month-year-liqpay"}},
      token_ref: {"tokenRef", {:string, :generic}},
      token_suffix: {"tokenSuffix", {:string, :generic}}
    ]
  end
end
