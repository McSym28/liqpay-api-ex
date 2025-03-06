defmodule LiqPayAPI.Information.PaymentArchive.Response do
  @moduledoc """
  Provides struct and type for a Information.PaymentArchive.Response
  """

  @behaviour OpenAPIClient.Schema

  @type t :: %__MODULE__{
          data: [LiqPayAPI.Information.PaymentArchive.Response.Data.t()] | nil,
          result: :error | :ok | :success | String.t() | nil
        }
  @type types :: :t

  defstruct [:data, :result]

  @doc false
  @impl OpenAPIClient.Schema
  @spec __fields__(types()) :: keyword(OpenAPIClient.Schema.field_type())
  def __fields__(:t) do
    [
      data: {"data", [{LiqPayAPI.Information.PaymentArchive.Response.Data, :t}]},
      result:
        {"result", {:enum, [{:error, "error"}, {:ok, "ok"}, {:success, "success"}, :not_strict]}}
    ]
  end
end
