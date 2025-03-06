defmodule LiqPayAPI.Partnership.InfoUser.Request do
  @moduledoc """
  Provides struct and type for a Partnership.InfoUser.Request
  """

  @behaviour OpenAPIClient.Schema

  @type t :: %__MODULE__{
          action: :agent_info_user,
          phone: String.t(),
          public_key: String.t(),
          version: 3
        }
  @type types :: :t

  @enforce_keys [:phone]
  defstruct [:phone, :public_key, action: :agent_info_user, version: 3]

  @doc false
  @impl OpenAPIClient.Schema
  @spec __fields__(types()) :: keyword(OpenAPIClient.Schema.field_type())
  def __fields__(:t) do
    [
      action: {"action", {:enum, agent_info_user: "agent_info_user"}},
      phone: {"phone", {:string, :generic}},
      public_key:
        {"public_key", {:string, :generic},
         fn -> Application.get_env(:liqpay_api_ex, :public_key) end},
      version: {"version", {:enum, [3]}}
    ]
  end
end
