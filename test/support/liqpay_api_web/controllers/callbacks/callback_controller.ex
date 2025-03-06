defmodule LiqPayAPIWeb.Callbacks.CallbackController do
  use LiqPayAPIWeb, :controller

  plug(OpenAPIClient.Plugs.CallbackInitializer,
    implementation: {:mock, LiqPayAPI.CallbacksMock},
    behaviour: LiqPayAPI.Callbacks,
    function_name: :callback
  )

  plug(OpenAPIClient.Plugs.RequestTypedDecoder)
  plug(OpenAPIClient.Plugs.FunctionCallDecoder)
  plug(OpenAPIClient.Plugs.FunctionCall)
  plug(OpenAPIClient.Plugs.FunctionResultEncoder)
  plug(OpenAPIClient.Plugs.ResponseTypedEncoder)
  plug(OpenAPIClient.Plugs.ResponseSerializers, serializers: [json: [json_encoder: Jason]])

  @spec callback(conn :: Plug.Conn.t(), params :: Plug.Conn.params()) :: Plug.Conn.t()
  def callback(conn, _params) do
    Plug.Conn.send_resp(conn)
  end
end
