defmodule LiqPayAPI.Partnership.ShopCreate.Documents.Request do
  @moduledoc """
  Provides struct and type for a Partnership.ShopCreate.Documents.Request
  """

  @behaviour OpenAPIClient.Schema

  @type t :: %__MODULE__{
          action: :agent_info_mcc_docs,
          language: :en | :uk | nil,
          mcc_code: number,
          public_key: String.t(),
          version: 3
        }
  @type types :: :t

  @enforce_keys [:mcc_code]
  defstruct [:language, :mcc_code, :public_key, action: :agent_info_mcc_docs, version: 3]

  @doc false
  @impl OpenAPIClient.Schema
  @spec __fields__(types()) :: keyword(OpenAPIClient.Schema.schema_type())
  def __fields__(:t) do
    [
      action: {"action", {:enum, agent_info_mcc_docs: "agent_info_mcc_docs"}},
      language: {"language", {:enum, en: "en", uk: "uk"}},
      mcc_code: {"mcc_code", :number},
      public_key: {"public_key", {:string, :generic}},
      version: {"version", {:enum, [3]}}
    ]
  end
end
