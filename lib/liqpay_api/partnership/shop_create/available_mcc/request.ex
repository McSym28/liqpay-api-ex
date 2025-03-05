defmodule LiqPayAPI.Partnership.ShopCreate.AvailableMCC.Request do
  @moduledoc """
  Provides struct and type for a Partnership.ShopCreate.AvailableMCC.Request
  """

  @behaviour OpenAPIClient.Schema

  @type t :: %__MODULE__{
          action: :agent_info_mcc_codes,
          language: :en | :uk | nil,
          public_key: String.t(),
          version: 3
        }
  @type types :: :t

  defstruct [:language, :public_key, action: :agent_info_mcc_codes, version: 3]

  @doc false
  @impl OpenAPIClient.Schema
  @spec __fields__(types()) :: keyword(OpenAPIClient.Schema.schema_type())
  def __fields__(:t) do
    [
      action: {"action", {:enum, agent_info_mcc_codes: "agent_info_mcc_codes"}},
      language: {"language", {:enum, en: "en", uk: "uk"}},
      public_key: {"public_key", {:string, :generic}},
      version: {"version", {:enum, [3]}}
    ]
  end
end
