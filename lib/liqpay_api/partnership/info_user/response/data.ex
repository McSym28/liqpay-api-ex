defmodule LiqPayAPI.Partnership.InfoUser.Response.Data do
  @moduledoc """
  Provides struct and type for a Partnership.InfoUser.Response.Data
  """

  @behaviour OpenAPIClient.Schema

  @type t :: %__MODULE__{
          blocked: boolean | nil,
          create_date: DateTime.t() | nil,
          description: String.t() | nil,
          email: String.t() | nil,
          logo: String.t() | nil,
          name: String.t() | nil,
          public_key: String.t() | nil,
          role: String.t() | nil,
          update_date: DateTime.t() | nil,
          url: String.t() | nil
        }
  @type types :: :t

  defstruct [
    :blocked,
    :create_date,
    :description,
    :email,
    :logo,
    :name,
    :public_key,
    :role,
    :update_date,
    :url
  ]

  @doc false
  @impl OpenAPIClient.Schema
  @spec __fields__(types()) :: keyword(OpenAPIClient.Schema.schema_type())
  def __fields__(:t) do
    [
      blocked: {"blocked", :boolean},
      create_date: {"create_date", {:integer, "timestamp-ms"}},
      description: {"description", {:string, :generic}},
      email: {"email", {:string, :generic}},
      logo: {"logo", {:string, :generic}},
      name: {"name", {:string, :generic}},
      public_key: {"public_key", {:string, :generic}},
      role: {"role", {:string, :generic}},
      update_date: {"update_date", {:integer, "timestamp-ms"}},
      url: {"url", {:string, :generic}}
    ]
  end
end
