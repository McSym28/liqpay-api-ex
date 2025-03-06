defmodule LiqPayAPI.Partnership.ShopCreate.Register.Request.Aggregator.LawCTOInfo do
  @moduledoc """
  Provides struct and type for a Partnership.ShopCreate.Register.Request.Aggregator.LawCTOInfo
  """

  @behaviour OpenAPIClient.Schema

  @type t :: %__MODULE__{
          birth_date: Date.t() | nil,
          citizenship: String.t() | nil,
          inn: String.t() | nil,
          name: String.t() | nil,
          residency: String.t() | nil
        }
  @type types :: :t

  defstruct [:birth_date, :citizenship, :inn, :name, :residency]

  @doc false
  @impl OpenAPIClient.Schema
  @spec __fields__(types()) :: keyword(OpenAPIClient.Schema.field_type())
  def __fields__(:t) do
    [
      birth_date: {"birth_date", {:string, :date}},
      citizenship: {"citizenship", {:string, :generic}},
      inn: {"inn", {:string, :generic}},
      name: {"name", {:string, :generic}},
      residency: {"residency", {:string, :generic}}
    ]
  end
end
