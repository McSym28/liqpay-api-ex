defmodule LiqPayAPI.Partnership.ShopCreate.Register.Request do
  @moduledoc """
  Provides struct and type for a Partnership.ShopCreate.Register.Request
  """

  @behaviour OpenAPIClient.Schema

  @type t :: %__MODULE__{
          action: :agent_shop_register,
          aggregator: LiqPayAPI.Partnership.ShopCreate.Register.Request.Aggregator.t() | nil,
          amount_procent_agent: number | nil,
          amount_static_agent: number | nil,
          can_checkout_edit: boolean | nil,
          can_reports: boolean | nil,
          company: String.t(),
          currency_static_agent: String.t() | nil,
          description: String.t() | nil,
          docs: [LiqPayAPI.Partnership.ShopCreate.Register.Request.Docs.t()] | nil,
          email: String.t(),
          facebook: String.t() | nil,
          iban: String.t(),
          instagram: String.t() | nil,
          logo: String.t() | nil,
          mcc_code: integer,
          name: String.t(),
          okpo: String.t(),
          phone: String.t(),
          public_key: String.t(),
          telegram: String.t() | nil,
          url_app_android: String.t() | nil,
          url_app_iphone: String.t() | nil,
          url_callback_status: String.t() | nil,
          url_offer: String.t() | nil,
          url_site: String.t() | nil,
          version: 3,
          viber: String.t() | nil
        }
  @type types :: :t

  @enforce_keys [:company, :email, :iban, :mcc_code, :name, :okpo, :phone]
  defstruct [
    :aggregator,
    :amount_procent_agent,
    :amount_static_agent,
    :can_checkout_edit,
    :can_reports,
    :company,
    :currency_static_agent,
    :description,
    :docs,
    :email,
    :facebook,
    :iban,
    :instagram,
    :logo,
    :mcc_code,
    :name,
    :okpo,
    :phone,
    :public_key,
    :telegram,
    :url_app_android,
    :url_app_iphone,
    :url_callback_status,
    :url_offer,
    :url_site,
    :viber,
    action: :agent_shop_register,
    version: 3
  ]

  @doc false
  @impl OpenAPIClient.Schema
  @spec __fields__(types()) :: keyword(OpenAPIClient.Schema.field_type())
  def __fields__(:t) do
    [
      action: {"action", {:enum, agent_shop_register: "agent_shop_register"}},
      aggregator:
        {"aggregator", {LiqPayAPI.Partnership.ShopCreate.Register.Request.Aggregator, :t}},
      amount_procent_agent: {"amount_procent_agent", :number},
      amount_static_agent: {"amount_static_agent", :number},
      can_checkout_edit: {"can_checkout_edit", :boolean},
      can_reports: {"can_reports", :boolean},
      company: {"company", {:string, :generic}},
      currency_static_agent: {"currency_static_agent", {:string, :generic}},
      description: {"description", {:string, :generic}},
      docs: {"docs", [{LiqPayAPI.Partnership.ShopCreate.Register.Request.Docs, :t}]},
      email: {"email", {:string, :generic}},
      facebook: {"facebook", {:string, :generic}},
      iban: {"iban", {:string, :generic}},
      instagram: {"instagram", {:string, :generic}},
      logo: {"logo", {:string, :generic}},
      mcc_code: {"mcc_code", :integer},
      name: {"name", {:string, :generic}},
      okpo: {"okpo", {:string, :generic}},
      phone: {"phone", {:string, :generic}},
      public_key:
        {"public_key", {:string, :generic},
         fn -> Application.get_env(:liqpay_api_ex, :public_key) end},
      telegram: {"telegram", {:string, :generic}},
      url_app_android: {"url_app_android", {:string, :generic}},
      url_app_iphone: {"url_app_iphone", {:string, :generic}},
      url_callback_status: {"url_callback_status", {:string, :generic}},
      url_offer: {"url_offer", {:string, :generic}},
      url_site: {"url_site", {:string, :generic}},
      version: {"version", {:enum, [3]}},
      viber: {"viber", {:string, :generic}}
    ]
  end
end
