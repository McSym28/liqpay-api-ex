defmodule LiqPayAPI.Information.Register.CompensationReportP2P.Response do
  @moduledoc """
  Provides struct and type for a Information.Register.CompensationReportP2P.Response
  """

  @behaviour OpenAPIClient.Schema

  @type t :: %__MODULE__{
          register_token: String.t(),
          result: :error | :ok | String.t(),
          status: String.t()
        }
  @type types :: :t

  @enforce_keys [:register_token, :result, :status]
  defstruct [:register_token, :result, :status]

  @doc false
  @impl OpenAPIClient.Schema
  @spec __fields__(types()) :: keyword(OpenAPIClient.Schema.field_type())
  def __fields__(:t) do
    [
      register_token: {"register_token", {:string, :generic}},
      result: {"result", {:enum, [{:error, "error"}, {:ok, "ok"}, :not_strict]}},
      status: {"status", {:string, :generic}}
    ]
  end
end
