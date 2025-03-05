defmodule LiqPayAPI.Partnership.ShopCreate.Documents.Response do
  @moduledoc """
  Provides struct and type for a Partnership.ShopCreate.Documents.Response
  """

  @behaviour OpenAPIClient.Schema

  @type t :: %__MODULE__{
          expected_docs:
            [LiqPayAPI.Partnership.ShopCreate.Documents.Response.ExpectedDocs.t()] | nil,
          result: :error | :ok | String.t() | nil,
          status: String.t() | nil
        }
  @type types :: :t

  defstruct [:expected_docs, :result, :status]

  @doc false
  @impl OpenAPIClient.Schema
  @spec __fields__(types()) :: keyword(OpenAPIClient.Schema.schema_type())
  def __fields__(:t) do
    [
      expected_docs:
        {"expected_docs",
         [{LiqPayAPI.Partnership.ShopCreate.Documents.Response.ExpectedDocs, :t}]},
      result: {"result", {:enum, [{:error, "error"}, {:ok, "ok"}, :not_strict]}},
      status: {"status", {:string, :generic}}
    ]
  end
end
