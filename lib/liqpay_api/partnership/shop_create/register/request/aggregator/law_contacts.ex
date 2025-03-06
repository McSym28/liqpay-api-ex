defmodule LiqPayAPI.Partnership.ShopCreate.Register.Request.Aggregator.LawContacts do
  @moduledoc """
  Provides struct and type for a Partnership.ShopCreate.Register.Request.Aggregator.LawContacts
  """

  @behaviour OpenAPIClient.Schema

  @type t :: %__MODULE__{email: String.t() | nil, phone: String.t() | nil}
  @type types :: :t

  defstruct [:email, :phone]

  @doc false
  @impl OpenAPIClient.Schema
  @spec __fields__(types()) :: keyword(OpenAPIClient.Schema.field_type())
  def __fields__(:t) do
    [email: {"email", {:string, :generic}}, phone: {"phone", {:string, :generic}}]
  end
end
