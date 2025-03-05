defmodule LiqPayAPI.Partnership.InfoUser.Response do
  @moduledoc """
  Provides struct and type for a Partnership.InfoUser.Response
  """

  @behaviour OpenAPIClient.Schema

  @type t :: %__MODULE__{
          data: [LiqPayAPI.Partnership.InfoUser.Response.Data.t()] | nil,
          result: :error | :ok | String.t() | nil
        }
  @type types :: :t

  defstruct [:data, :result]

  @doc false
  @impl OpenAPIClient.Schema
  @spec __fields__(types()) :: keyword(OpenAPIClient.Schema.schema_type())
  def __fields__(:t) do
    [
      data: {"data", [{LiqPayAPI.Partnership.InfoUser.Response.Data, :t}]},
      result: {"result", {:enum, [{:error, "error"}, {:ok, "ok"}, :not_strict]}}
    ]
  end
end
