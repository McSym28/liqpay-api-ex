defmodule LiqPayAPI.Partnership.ShopCreate.Create.Request.Aggregator.LawOwnersInfo do
  @moduledoc """
  Provides struct and type for a Partnership.ShopCreate.Create.Request.Aggregator.LawOwnersInfo
  """

  @behaviour OpenAPIClient.Schema

  @type t :: %__MODULE__{
          birth_date: Date.t() | nil,
          citizenship: String.t() | nil,
          inn: String.t() | nil,
          name: String.t() | nil,
          residency: String.t() | nil,
          share_in_capital: String.t() | nil
        }
  @type types :: :t

  defstruct [:birth_date, :citizenship, :inn, :name, :residency, :share_in_capital]

  @doc false
  @impl OpenAPIClient.Schema
  @spec __fields__(types()) :: keyword(OpenAPIClient.Schema.schema_type())
  def __fields__(:t) do
    [
      birth_date: {"birth_date", {:string, :date}},
      citizenship: {"citizenship", {:string, :generic}},
      inn: {"inn", {:string, :generic}},
      name: {"name", {:string, :generic}},
      residency: {"residency", {:string, :generic}},
      share_in_capital: {"share_in_capital", {:string, :generic}}
    ]
  end
end
