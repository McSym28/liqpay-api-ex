defmodule LiqPayAPI.InternetAcquiring.GPay.DecryptedToken.Response do
  @moduledoc """
  Provides struct and type for a InternetAcquiring.GPay.DecryptedToken.Response
  """

  @behaviour OpenAPIClient.Schema

  @type t :: %__MODULE__{
          confirm_phone: String.t() | nil,
          language: String.t() | nil,
          result: :error | :ok | String.t() | nil
        }
  @type types :: :t

  defstruct [:confirm_phone, :language, :result]

  @doc false
  @impl OpenAPIClient.Schema
  @spec __fields__(types()) :: keyword(OpenAPIClient.Schema.field_type())
  def __fields__(:t) do
    [
      confirm_phone: {"confirm_phone", {:string, :generic}},
      language: {"language", {:string, :generic}},
      result: {"result", {:enum, [{:error, "error"}, {:ok, "ok"}, :not_strict]}}
    ]
  end
end
