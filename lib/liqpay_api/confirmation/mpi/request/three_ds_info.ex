defmodule LiqPayAPI.Confirmation.MPI.Request.ThreeDSInfo do
  @moduledoc """
  Provides struct and type for a Confirmation.MPI.Request.ThreeDSInfo
  """

  @behaviour OpenAPIClient.Schema

  @type t :: %__MODULE__{
          browser_accept_header: String.t(),
          browser_color_depth: String.t(),
          browser_java_enabled: boolean | nil,
          browser_javascript_enabled: boolean | nil,
          browser_language: String.t(),
          browser_screen_height: String.t(),
          browser_screen_width: String.t(),
          browser_tz: String.t(),
          browser_user_agent: String.t(),
          notification_url: String.t(),
          three_ds_requestor_url: String.t()
        }
  @type types :: :t

  @enforce_keys [
    :browser_accept_header,
    :browser_color_depth,
    :browser_language,
    :browser_screen_height,
    :browser_screen_width,
    :browser_tz,
    :browser_user_agent,
    :notification_url,
    :three_ds_requestor_url
  ]
  defstruct [
    :browser_accept_header,
    :browser_color_depth,
    :browser_java_enabled,
    :browser_javascript_enabled,
    :browser_language,
    :browser_screen_height,
    :browser_screen_width,
    :browser_tz,
    :browser_user_agent,
    :notification_url,
    :three_ds_requestor_url
  ]

  @doc false
  @impl OpenAPIClient.Schema
  @spec __fields__(types()) :: keyword(OpenAPIClient.Schema.schema_type())
  def __fields__(:t) do
    [
      browser_accept_header: {"browserAcceptHeader", {:string, :generic}},
      browser_color_depth: {"browserColorDepth", {:string, :generic}},
      browser_java_enabled: {"browserJavaEnabled", :boolean},
      browser_javascript_enabled: {"browserJavascriptEnabled", :boolean},
      browser_language: {"browserLanguage", {:string, :generic}},
      browser_screen_height: {"browserScreenHeight", {:string, :generic}},
      browser_screen_width: {"browserScreenWidth", {:string, :generic}},
      browser_tz: {"browserTZ", {:string, :generic}},
      browser_user_agent: {"browserUserAgent", {:string, :generic}},
      notification_url: {"notificationURL", {:string, :generic}},
      three_ds_requestor_url: {"threeDSRequestorURL", {:string, :generic}}
    ]
  end
end
