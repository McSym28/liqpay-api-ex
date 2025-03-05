defmodule LiqPayAPI.InternetAcquiring.Invoice.Units.Request do
  @moduledoc """
  Provides struct and type for a InternetAcquiring.Invoice.Units.Request
  """

  @behaviour OpenAPIClient.Schema

  @type t :: %__MODULE__{
          action: :invoice_units_get_list | :invoice_units_get_list_by_lang,
          hide_name_lang: boolean | nil,
          language: :en | :uk | nil,
          version: 3
        }
  @type types :: :t

  @enforce_keys [:action]
  defstruct [:action, :hide_name_lang, :language, version: 3]

  @doc false
  @impl OpenAPIClient.Schema
  @spec __fields__(types()) :: keyword(OpenAPIClient.Schema.schema_type())
  def __fields__(:t) do
    [
      action:
        {"action",
         {:enum,
          invoice_units_get_list: "invoice_units_get_list",
          invoice_units_get_list_by_lang: "invoice_units_get_list_by_lang"}},
      hide_name_lang: {"hide_name_lang", :boolean},
      language: {"language", {:enum, en: "en", uk: "uk"}},
      version: {"version", {:enum, [3]}}
    ]
  end
end
