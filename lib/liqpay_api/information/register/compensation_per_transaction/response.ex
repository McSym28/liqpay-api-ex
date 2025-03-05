defmodule LiqPayAPI.Information.Register.CompensationPerTransaction.Response do
  @moduledoc """
  Provides struct and type for a Information.Register.CompensationPerTransaction.Response
  """

  @behaviour OpenAPIClient.Schema

  @type t :: %__MODULE__{
          data:
            [LiqPayAPI.Information.Register.CompensationPerTransaction.Response.Data.t()] | nil,
          result: :error | :ok | :success | String.t() | nil
        }
  @type types :: :t

  defstruct [:data, :result]

  @doc false
  @impl OpenAPIClient.Schema
  @spec __fields__(types()) :: keyword(OpenAPIClient.Schema.schema_type())
  def __fields__(:t) do
    [
      data:
        {"data", [{LiqPayAPI.Information.Register.CompensationPerTransaction.Response.Data, :t}]},
      result:
        {"result", {:enum, [{:error, "error"}, {:ok, "ok"}, {:success, "success"}, :not_strict]}}
    ]
  end
end
