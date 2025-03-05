defmodule LiqPayAPI.Partnership.ShopEdit.Request do
  @moduledoc """
  Provides struct and type for a Partnership.ShopEdit.Request
  """

  @behaviour OpenAPIClient.Schema

  @type t :: %__MODULE__{
          action: :agent_shop_edit,
          amount_procent_agent: number | nil,
          amount_static_agent: number | nil,
          can_checkout_edit: boolean | nil,
          can_reports: boolean | nil,
          company: String.t() | nil,
          currency_static_agent: String.t() | nil,
          description: String.t(),
          email: String.t(),
          iban: String.t() | nil,
          logo: String.t() | nil,
          merchant_public_key: String.t(),
          name: String.t(),
          okpo: String.t() | nil,
          phone: String.t(),
          public_key: String.t(),
          public_phone: String.t() | nil,
          site: String.t(),
          version: 3
        }
  @type types :: :t

  @enforce_keys [:description, :email, :merchant_public_key, :name, :phone, :site]
  defstruct [
    :amount_procent_agent,
    :amount_static_agent,
    :can_checkout_edit,
    :can_reports,
    :company,
    :currency_static_agent,
    :description,
    :email,
    :iban,
    :logo,
    :merchant_public_key,
    :name,
    :okpo,
    :phone,
    :public_key,
    :public_phone,
    :site,
    action: :agent_shop_edit,
    version: 3
  ]

  @doc false
  @impl OpenAPIClient.Schema
  @spec __fields__(types()) :: keyword(OpenAPIClient.Schema.schema_type())
  def __fields__(:t) do
    [
      action: {"action", {:enum, agent_shop_edit: "agent_shop_edit"}},
      amount_procent_agent: {"amount_procent_agent", :number},
      amount_static_agent: {"amount_static_agent", :number},
      can_checkout_edit: {"can_checkout_edit", :boolean},
      can_reports: {"can_reports", :boolean},
      company: {"company", {:string, :generic}},
      currency_static_agent: {"currency_static_agent", {:string, :generic}},
      description: {"description", {:string, :generic}},
      email: {"email", {:string, :generic}},
      iban: {"iban", {:string, :generic}},
      logo: {"logo", {:string, :generic}},
      merchant_public_key: {"merchant_public_key", {:string, :generic}},
      name: {"name", {:string, :generic}},
      okpo: {"okpo", {:string, :generic}},
      phone: {"phone", {:string, :generic}},
      public_key: {"public_key", {:string, :generic}},
      public_phone: {"public_phone", {:string, :generic}},
      site: {"site", {:string, :generic}},
      version: {"version", {:enum, [3]}}
    ]
  end
end
