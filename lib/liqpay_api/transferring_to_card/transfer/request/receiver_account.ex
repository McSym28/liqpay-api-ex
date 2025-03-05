defmodule LiqPayAPI.TransferringToCard.Transfer.Request.ReceiverAccount do
  @moduledoc """
  Provides struct and type for a TransferringToCard.Transfer.Request.ReceiverAccount
  """

  @behaviour OpenAPIClient.Schema

  @type t :: %__MODULE__{
          receiver_account: String.t(),
          receiver_company: String.t(),
          receiver_mfo: String.t(),
          receiver_okpo: String.t()
        }
  @type types :: :t

  @enforce_keys [:receiver_account, :receiver_company, :receiver_mfo, :receiver_okpo]
  defstruct [:receiver_account, :receiver_company, :receiver_mfo, :receiver_okpo]

  @doc false
  @impl OpenAPIClient.Schema
  @spec __fields__(types()) :: keyword(OpenAPIClient.Schema.schema_type())
  def __fields__(:t) do
    [
      receiver_account: {"receiver_account", {:string, :generic}},
      receiver_company: {"receiver_company", {:string, :generic}},
      receiver_mfo: {"receiver_mfo", {:string, :generic}},
      receiver_okpo: {"receiver_okpo", {:string, :generic}}
    ]
  end
end
