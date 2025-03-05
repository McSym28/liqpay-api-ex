defmodule LiqPayAPI.Partnership.ShopCreate.AvailableMCC.Response do
  @moduledoc """
  Provides struct and type for a Partnership.ShopCreate.AvailableMCC.Response
  """

  @behaviour OpenAPIClient.Schema

  @type t :: %__MODULE__{
          mcc_codes: [LiqPayAPI.Partnership.ShopCreate.AvailableMCC.Response.MCCCodes.t()] | nil,
          result: :error | :ok | String.t() | nil,
          status: String.t() | nil
        }
  @type types :: :t

  defstruct [:mcc_codes, :result, :status]

  @doc false
  @impl OpenAPIClient.Schema
  @spec __fields__(types()) :: keyword(OpenAPIClient.Schema.schema_type())
  def __fields__(:t) do
    [
      mcc_codes:
        {"mcc_codes", [{LiqPayAPI.Partnership.ShopCreate.AvailableMCC.Response.MCCCodes, :t}]},
      result: {"result", {:enum, [{:error, "error"}, {:ok, "ok"}, :not_strict]}},
      status: {"status", {:string, :generic}}
    ]
  end
end
