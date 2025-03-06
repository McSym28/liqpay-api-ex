defmodule LiqPayAPI.InternetAcquiring.TwoStep.Complete.Request.RROInfo do
  @moduledoc """
  Provides struct and type for a InternetAcquiring.TwoStep.Complete.Request.RROInfo
  """

  @behaviour OpenAPIClient.Schema

  @type t :: %__MODULE__{
          delivery_emails: [String.t()] | nil,
          items: [LiqPayAPI.InternetAcquiring.TwoStep.Complete.Request.RROInfo.Items.t()] | nil
        }
  @type types :: :t

  defstruct [:delivery_emails, :items]

  @doc false
  @impl OpenAPIClient.Schema
  @spec __fields__(types()) :: keyword(OpenAPIClient.Schema.field_type())
  def __fields__(:t) do
    [
      delivery_emails: {"delivery_emails", string: :email},
      items: {"items", [{LiqPayAPI.InternetAcquiring.TwoStep.Complete.Request.RROInfo.Items, :t}]}
    ]
  end
end
