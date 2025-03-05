defmodule LiqPayAPI.Partnership.ShopCreate.Register.Response do
  @moduledoc """
  Provides struct and type for a Partnership.ShopCreate.Register.Response
  """

  @behaviour OpenAPIClient.Schema

  @type t :: %__MODULE__{
          activation_status: number | nil,
          company_name: String.t() | nil,
          create_date: Date.t() | nil,
          email: String.t() | nil,
          name: String.t() | nil,
          okpo: String.t() | nil,
          phone: String.t() | nil,
          public_key: String.t() | nil,
          refund_number: String.t() | nil,
          update_date: Date.t() | nil,
          url: String.t() | nil
        }
  @type types :: :t

  defstruct [
    :activation_status,
    :company_name,
    :create_date,
    :email,
    :name,
    :okpo,
    :phone,
    :public_key,
    :refund_number,
    :update_date,
    :url
  ]

  @doc false
  @impl OpenAPIClient.Schema
  @spec __fields__(types()) :: keyword(OpenAPIClient.Schema.schema_type())
  def __fields__(:t) do
    [
      activation_status: {"activation_status", :number},
      company_name: {"company_name", {:string, :generic}},
      create_date: {"create_date", {:string, :date}},
      email: {"email", {:string, :generic}},
      name: {"name", {:string, :generic}},
      okpo: {"okpo", {:string, :generic}},
      phone: {"phone", {:string, :generic}},
      public_key: {"public_key", {:string, :generic}},
      refund_number: {"refund_number", {:string, :generic}},
      update_date: {"update_date", {:string, :date}},
      url: {"url", {:string, :generic}}
    ]
  end
end
