defmodule LiqPayAPI.Partnership.InfoMerchant.Request do
  @moduledoc """
  Provides struct and type for a Partnership.InfoMerchant.Request
  """

  @behaviour OpenAPIClient.Schema

  @type t :: %__MODULE__{
          action: :agent_info_merchant,
          language: :en | :uk | nil,
          merchant_public_key: String.t(),
          public_key: String.t(),
          version: 3
        }
  @type types :: :t

  @enforce_keys [:merchant_public_key]
  defstruct [
    :language,
    :merchant_public_key,
    :public_key,
    action: :agent_info_merchant,
    version: 3
  ]

  @doc false
  @impl OpenAPIClient.Schema
  @spec __fields__(types()) :: keyword(OpenAPIClient.Schema.field_type())
  def __fields__(:t) do
    [
      action: {"action", {:enum, agent_info_merchant: "agent_info_merchant"}},
      language: {"language", {:enum, en: "en", uk: "uk"}},
      merchant_public_key: {"merchant_public_key", {:string, :generic}},
      public_key:
        {"public_key", {:string, :generic},
         fn -> Application.get_env(:liqpay_api_ex, :public_key) end},
      version: {"version", {:enum, [3]}}
    ]
  end
end
