defmodule LiqPayAPI.Partnership.ShopCreate.AvailableMCC.Response.MCCCodes do
  @moduledoc """
  Provides struct and type for a Partnership.ShopCreate.AvailableMCC.Response.MCCCodes
  """

  @behaviour OpenAPIClient.Schema

  @type t :: %__MODULE__{
          id: integer | nil,
          mcc_code: integer | nil,
          name: String.t() | nil,
          parent_id: integer | nil
        }
  @type types :: :t

  defstruct [:id, :mcc_code, :name, :parent_id]

  @doc false
  @impl OpenAPIClient.Schema
  @spec __fields__(types()) :: keyword(OpenAPIClient.Schema.field_type())
  def __fields__(:t) do
    [
      id: {"id", :integer},
      mcc_code: {"mcc_code", :integer},
      name: {"name", {:string, :generic}},
      parent_id: {"parent_id", :integer}
    ]
  end
end
