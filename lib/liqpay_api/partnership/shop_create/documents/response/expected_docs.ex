defmodule LiqPayAPI.Partnership.ShopCreate.Documents.Response.ExpectedDocs do
  @moduledoc """
  Provides struct and type for a Partnership.ShopCreate.Documents.Response.ExpectedDocs
  """

  @behaviour OpenAPIClient.Schema

  @type t :: %__MODULE__{
          alt_docs: [String.t()] | nil,
          description: String.t() | nil,
          doc_id: number | nil,
          doc_type: String.t() | nil,
          name: String.t() | nil
        }
  @type types :: :t

  defstruct [:alt_docs, :description, :doc_id, :doc_type, :name]

  @doc false
  @impl OpenAPIClient.Schema
  @spec __fields__(types()) :: keyword(OpenAPIClient.Schema.schema_type())
  def __fields__(:t) do
    [
      alt_docs: {"alt_docs", string: :email},
      description: {"description", {:string, :generic}},
      doc_id: {"doc_id", :number},
      doc_type: {"doc_type", {:string, :generic}},
      name: {"name", {:string, :generic}}
    ]
  end
end
