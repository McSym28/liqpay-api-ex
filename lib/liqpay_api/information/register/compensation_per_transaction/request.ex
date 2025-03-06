defmodule LiqPayAPI.Information.Register.CompensationPerTransaction.Request do
  @moduledoc """
  Provides struct and type for a Information.Register.CompensationPerTransaction.Request
  """

  @behaviour OpenAPIClient.Schema

  @type t :: %__MODULE__{
          action: :register,
          date: Date.t(),
          format: String.t(),
          public_key: String.t(),
          version: 3
        }
  @type types :: :t

  @enforce_keys [:date, :format]
  defstruct [:date, :format, :public_key, action: :register, version: 3]

  @doc false
  @impl OpenAPIClient.Schema
  @spec __fields__(types()) :: keyword(OpenAPIClient.Schema.field_type())
  def __fields__(:t) do
    [
      action: {"action", {:enum, register: "register"}},
      date: {"date", {:string, :date}},
      format: {"format", {:string, :generic}},
      public_key:
        {"public_key", {:string, :generic},
         fn -> Application.get_env(:liqpay_api_ex, :public_key) end},
      version: {"version", {:enum, [3]}}
    ]
  end
end
