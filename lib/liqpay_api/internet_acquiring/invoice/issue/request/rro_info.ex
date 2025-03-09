defmodule LiqPayAPI.InternetAcquiring.Invoice.Issue.Request.RroInfo do
  @moduledoc """
  Provides struct and type for a InternetAcquiring.Invoice.Issue.Request.RroInfo
  """

  @behaviour OpenAPIClient.Schema

  @type t :: %__MODULE__{
          delivery_emails: [String.t()] | nil,
          items: [LiqPayAPI.InternetAcquiring.Invoice.Issue.Request.RroInfoItems.t()] | nil
        }
  @type types :: :t

  defstruct [:delivery_emails, :items]

  @doc false
  @impl OpenAPIClient.Schema
  @spec __fields__(types()) :: keyword(OpenAPIClient.Schema.field_type())
  def __fields__(:t) do
    [
      delivery_emails: {"delivery_emails", string: :email},
      items: {"items", [{LiqPayAPI.InternetAcquiring.Invoice.Issue.Request.RroInfoItems, :t}]}
    ]
  end
end
