defmodule LiqPayAPI.Partnership.InfoMerchant.Response do
  @moduledoc """
  Provides struct and type for a Partnership.InfoMerchant.Response
  """

  @behaviour OpenAPIClient.Schema

  @type t :: %__MODULE__{
          blocked: boolean | nil,
          category: String.t() | nil,
          comment: String.t() | nil,
          company_name: String.t() | nil,
          create_date: DateTime.t() | nil,
          description: String.t() | nil,
          email: String.t() | nil,
          link: String.t() | nil,
          logo: String.t() | nil,
          mcc: String.t() | nil,
          mfo: String.t() | nil,
          name: String.t() | nil,
          okpo: String.t() | nil,
          phone: String.t() | nil,
          public_key: String.t() | nil,
          public_phone: String.t() | nil,
          refund_number: String.t() | nil,
          refund_way: String.t() | nil,
          result: :error | :ok | String.t() | nil,
          status: String.t() | nil,
          status_description: String.t() | nil,
          update_date: DateTime.t() | nil,
          url: String.t() | nil
        }
  @type types :: :t

  defstruct [
    :blocked,
    :category,
    :comment,
    :company_name,
    :create_date,
    :description,
    :email,
    :link,
    :logo,
    :mcc,
    :mfo,
    :name,
    :okpo,
    :phone,
    :public_key,
    :public_phone,
    :refund_number,
    :refund_way,
    :result,
    :status,
    :status_description,
    :update_date,
    :url
  ]

  @doc false
  @impl OpenAPIClient.Schema
  @spec __fields__(types()) :: keyword(OpenAPIClient.Schema.field_type())
  def __fields__(:t) do
    [
      blocked: {"blocked", :boolean},
      category: {"category", {:string, :generic}},
      comment: {"comment", {:string, :generic}},
      company_name: {"company_name", {:string, :generic}},
      create_date: {"create_date", {:integer, "timestamp-ms"}},
      description: {"description", {:string, :generic}},
      email: {"email", {:string, :generic}},
      link: {"link", {:string, :generic}},
      logo: {"logo", {:string, :generic}},
      mcc: {"mcc", {:string, :generic}},
      mfo: {"mfo", {:string, :generic}},
      name: {"name", {:string, :generic}},
      okpo: {"okpo", {:string, :generic}},
      phone: {"phone", {:string, :generic}},
      public_key: {"public_key", {:string, :generic}},
      public_phone: {"public_phone", {:string, :generic}},
      refund_number: {"refund_number", {:string, :generic}},
      refund_way: {"refund_way", {:string, :generic}},
      result: {"result", {:enum, [{:error, "error"}, {:ok, "ok"}, :not_strict]}},
      status: {"status", {:string, :generic}},
      status_description: {"status_description", {:string, :generic}},
      update_date: {"update_date", {:integer, "timestamp-ms"}},
      url: {"url", {:string, :generic}}
    ]
  end
end
