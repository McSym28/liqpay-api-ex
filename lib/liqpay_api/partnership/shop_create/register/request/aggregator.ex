defmodule LiqPayAPI.Partnership.ShopCreate.Register.Request.Aggregator do
  @moduledoc """
  Provides struct and type for a Partnership.ShopCreate.Register.Request.Aggregator
  """

  @behaviour OpenAPIClient.Schema

  @type t :: %__MODULE__{
          law_co_owners_info: [
            LiqPayAPI.Partnership.ShopCreate.Register.Request.Aggregator.LawCoOwnersInfo.t()
          ],
          law_contacts:
            LiqPayAPI.Partnership.ShopCreate.Register.Request.Aggregator.LawContacts.t(),
          law_cto_info:
            LiqPayAPI.Partnership.ShopCreate.Register.Request.Aggregator.LawCTOInfo.t(),
          law_iban: String.t(),
          law_name: String.t(),
          law_okpo: String.t(),
          law_owners_info: [
            LiqPayAPI.Partnership.ShopCreate.Register.Request.Aggregator.LawOwnersInfo.t()
          ]
        }
  @type types :: :t

  @enforce_keys [
    :law_co_owners_info,
    :law_contacts,
    :law_cto_info,
    :law_iban,
    :law_name,
    :law_okpo,
    :law_owners_info
  ]
  defstruct [
    :law_co_owners_info,
    :law_contacts,
    :law_cto_info,
    :law_iban,
    :law_name,
    :law_okpo,
    :law_owners_info
  ]

  @doc false
  @impl OpenAPIClient.Schema
  @spec __fields__(types()) :: keyword(OpenAPIClient.Schema.field_type())
  def __fields__(:t) do
    [
      law_co_owners_info:
        {"law_co_owners_info",
         [{LiqPayAPI.Partnership.ShopCreate.Register.Request.Aggregator.LawCoOwnersInfo, :t}]},
      law_contacts:
        {"law_contacts",
         {LiqPayAPI.Partnership.ShopCreate.Register.Request.Aggregator.LawContacts, :t}},
      law_cto_info:
        {"law_cto_info",
         {LiqPayAPI.Partnership.ShopCreate.Register.Request.Aggregator.LawCTOInfo, :t}},
      law_iban: {"law_iban", {:string, :generic}},
      law_name: {"law_name", {:string, :generic}},
      law_okpo: {"law_okpo", {:string, :generic}},
      law_owners_info:
        {"law_owners_info",
         [{LiqPayAPI.Partnership.ShopCreate.Register.Request.Aggregator.LawOwnersInfo, :t}]}
    ]
  end
end
