defmodule LiqPayAPI.Callbacks do
  @moduledoc """
  Provides API callback related to callbacks
  """

  @behaviour OpenAPIClient.Callback

  @type callback_functions :: :callback

  @doc """
  Callback

  ## Arguments

    * `body`

  """
  @callback callback(LiqPayAPI.Callbacks.CallbackRequest.t()) ::
              :ok | {:error, OpenAPIClient.Error.t()}

  @optional_callbacks callback: 1

  @doc false
  @impl OpenAPIClient.Callback
  @spec __functions__(callback_functions()) :: [OpenAPIClient.Callback.function_option()]
  def __functions__(:callback) do
    [
      request_path_mask: "/{*request.body.server_url*}",
      request_types: [{"application/json", {LiqPayAPI.Callbacks.CallbackRequest, :t}}],
      response_types: [{200, :null}],
      profile: :default
    ]
  end
end
