defmodule LiqPayAPI.Partnership.ShopCreate.AvailableMCC.Response.MCCCodes do
  @moduledoc """
  Provides struct and type for a Partnership.ShopCreate.AvailableMCC.Response.MCCCodes
  """

  @behaviour OpenAPIClient.Schema

  @type t :: %__MODULE__{
          id: number | nil,
          mcc_code: number | nil,
          name: String.t() | nil,
          parent_id: number | nil
        }
  @type types :: :t

  defstruct [:id, :mcc_code, :name, :parent_id]

  @doc false
  @impl OpenAPIClient.Schema
  @spec __fields__(types()) :: keyword(OpenAPIClient.Schema.schema_type())
  def __fields__(:t) do
    [
      id: {"id", :number},
      mcc_code: {"mcc_code", :number},
      name: {"name", {:string, :generic}},
      parent_id: {"parent_id", :number}
    ]
  end
end
