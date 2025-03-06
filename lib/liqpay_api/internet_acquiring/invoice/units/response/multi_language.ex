defmodule LiqPayAPI.InternetAcquiring.Invoice.Units.Response.MultiLanguage do
  @moduledoc """
  Provides struct and type for a InternetAcquiring.Invoice.Units.Response.MultiLanguage
  """

  @behaviour OpenAPIClient.Schema

  @type t :: %__MODULE__{
          full_name_en: String.t() | nil,
          full_name_uk: String.t() | nil,
          id: String.t() | nil,
          short_name_en: String.t() | nil,
          short_name_uk: String.t() | nil
        }
  @type types :: :t

  defstruct [:full_name_en, :full_name_uk, :id, :short_name_en, :short_name_uk]

  @doc false
  @impl OpenAPIClient.Schema
  @spec __fields__(types()) :: keyword(OpenAPIClient.Schema.field_type())
  def __fields__(:t) do
    [
      full_name_en: {"full_name_en", {:string, :generic}},
      full_name_uk: {"full_name_uk", {:string, :generic}},
      id: {"id", {:string, :generic}},
      short_name_en: {"short_name_en", {:string, :generic}},
      short_name_uk: {"short_name_uk", {:string, :generic}}
    ]
  end
end
