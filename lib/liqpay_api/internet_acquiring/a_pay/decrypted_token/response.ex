defmodule LiqPayAPI.InternetAcquiring.APay.DecryptedToken.Response do
  @moduledoc """
  Provides struct and type for a InternetAcquiring.APay.DecryptedToken.Response
  """

  @behaviour OpenAPIClient.Schema

  @type t :: %__MODULE__{
          authcode_debit: String.t() | nil,
          language: String.t() | nil,
          public_key: String.t() | nil,
          result: :error | :ok | String.t() | nil
        }
  @type types :: :t

  defstruct [:authcode_debit, :language, :public_key, :result]

  @doc false
  @impl OpenAPIClient.Schema
  @spec __fields__(types()) :: keyword(OpenAPIClient.Schema.field_type())
  def __fields__(:t) do
    [
      authcode_debit: {"authcode_debit", {:string, :generic}},
      language: {"language", {:string, :generic}},
      public_key: {"public_key", {:string, :generic}},
      result: {"result", {:enum, [{:error, "error"}, {:ok, "ok"}, :not_strict]}}
    ]
  end
end
