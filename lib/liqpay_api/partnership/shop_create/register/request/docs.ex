defmodule LiqPayAPI.Partnership.ShopCreate.Register.Request.Docs do
  @moduledoc """
  Provides struct and type for a Partnership.ShopCreate.Register.Request.Docs
  """

  @behaviour OpenAPIClient.Schema

  @type t :: %__MODULE__{
          doc_id: number | nil,
          file: String.t(),
          file_name: String.t(),
          name: String.t()
        }
  @type types :: :t

  @enforce_keys [:file, :file_name, :name]
  defstruct [:doc_id, :file, :file_name, :name]

  @doc false
  @impl OpenAPIClient.Schema
  @spec __fields__(types()) :: keyword(OpenAPIClient.Schema.schema_type())
  def __fields__(:t) do
    [
      doc_id: {"doc_id", :number},
      file: {"file", {:string, :generic}},
      file_name: {"file_name", {:string, :generic}},
      name: {"name", {:string, :generic}}
    ]
  end
end
