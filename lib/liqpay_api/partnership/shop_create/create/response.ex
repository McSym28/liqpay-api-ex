defmodule LiqPayAPI.Partnership.ShopCreate.Create.Response do
  @moduledoc """
  Provides struct and type for a Partnership.ShopCreate.Create.Response
  """

  @behaviour OpenAPIClient.Schema

  @type t :: %__MODULE__{
          private_key: String.t() | nil,
          public_key: String.t() | nil,
          result: :error | :ok | String.t() | nil,
          status: :success | String.t() | nil
        }
  @type types :: :t

  defstruct [:private_key, :public_key, :result, :status]

  @doc false
  @impl OpenAPIClient.Schema
  @spec __fields__(types()) :: keyword(OpenAPIClient.Schema.field_type())
  def __fields__(:t) do
    [
      private_key: {"private_key", {:string, :generic}},
      public_key: {"public_key", {:string, :generic}},
      result: {"result", {:enum, [{:error, "error"}, {:ok, "ok"}, :not_strict]}},
      status: {"status", {:enum, [{:success, "success"}, :not_strict]}}
    ]
  end
end
