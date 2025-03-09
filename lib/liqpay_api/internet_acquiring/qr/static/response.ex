defmodule LiqPayAPI.InternetAcquiring.QR.Static.Response do
  @moduledoc """
  Provides struct and type for a InternetAcquiring.QR.Static.Response
  """

  @behaviour OpenAPIClient.Schema

  @type t :: %__MODULE__{
          amount: number | nil,
          create_date: DateTime.t() | nil,
          currency: String.t() | nil,
          description: String.t() | nil,
          final_date: DateTime.t() | nil,
          id: integer | nil,
          qrdata: String.t() | nil,
          shop_id: integer | nil,
          status: String.t() | nil,
          url: String.t() | nil
        }
  @type types :: :t

  defstruct [
    :amount,
    :create_date,
    :currency,
    :description,
    :final_date,
    :id,
    :qrdata,
    :shop_id,
    :status,
    :url
  ]

  @doc false
  @impl OpenAPIClient.Schema
  @spec __fields__(types()) :: keyword(OpenAPIClient.Schema.field_type())
  def __fields__(:t) do
    [
      amount: {"amount", :number},
      create_date: {"create_date", {:integer, "timestamp-s"}},
      currency: {"currency", {:string, :generic}},
      description: {"description", {:string, :generic}},
      final_date: {"final_date", {:integer, "timestamp-s"}},
      id: {"id", :integer},
      qrdata: {"qrdata", {:string, :generic}},
      shop_id: {"shop_id", :integer},
      status: {"status", {:string, :generic}},
      url: {"url", {:string, :generic}}
    ]
  end
end
