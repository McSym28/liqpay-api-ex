defmodule LiqPayAPI.Confirmation.OTP.Request do
  @moduledoc """
  Provides struct and type for a Confirmation.OTP.Request
  """

  @behaviour OpenAPIClient.Schema

  @type t :: %__MODULE__{
          action: :confirm,
          confirm_token: String.t(),
          otp: String.t(),
          public_key: String.t(),
          version: 3
        }
  @type types :: :t

  @enforce_keys [:confirm_token, :otp]
  defstruct [:confirm_token, :otp, :public_key, action: :confirm, version: 3]

  @doc false
  @impl OpenAPIClient.Schema
  @spec __fields__(types()) :: keyword(OpenAPIClient.Schema.field_type())
  def __fields__(:t) do
    [
      action: {"action", {:enum, confirm: "confirm"}},
      confirm_token: {"confirm_token", {:string, :generic}},
      otp: {"otp", {:string, :generic}},
      public_key:
        {"public_key", {:string, :generic},
         fn -> Application.get_env(:liqpay_api_ex, :public_key) end},
      version: {"version", {:enum, [3]}}
    ]
  end
end
