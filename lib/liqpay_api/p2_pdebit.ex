defmodule LiqPayAPI.P2Pdebit do
  @moduledoc """
  Provides API endpoint related to p2 pdebit
  """

  @base_url "https://www.liqpay.ua"

  @doc """
  Transfer of funds from cards to account

  ## Arguments

    * `body`

  ## Options

    * `base_url`: Request's base URL. Default value is taken from `@base_url`
    * `pipeline`: Operation pipeline for making a request. Default value obtained through a call to `OpenAPIClient.Utils.get_config(:default, :operation_pipeline)}
    * `client`: Module that implements `OpenAPIClient` behaviour. Default value obtained through a call to `OpenAPIClient.Utils.get_config(:default, :client, OpenAPIClient)`

  """
  @spec transfer(LiqPayAPI.P2PdebitTransfer.Request.t()) ::
          :ok | {:error, OpenAPIClient.Error.t()}
  @spec transfer(LiqPayAPI.P2PdebitTransfer.Request.t(), [
          {:base_url, String.t() | URI.t()}
          | {:pipeline, OpenAPIClient.pipeline()}
          | {:client, module()}
        ]) :: :ok | {:error, OpenAPIClient.Error.t()}
  def transfer(body, opts \\ []) do
    body =
      case body do
        %LiqPayAPI.P2PdebitTransfer.Request{public_key: nil} ->
          %LiqPayAPI.P2PdebitTransfer.Request{
            body
            | public_key: Application.get_env(:liqpay_api_ex, :public_key)
          }

        _ ->
          body
      end

    pipeline = opts[:pipeline] || OpenAPIClient.Utils.get_config(:default, :operation_pipeline)
    base_url = opts[:base_url] || @base_url
    client = opts[:client] || OpenAPIClient.Utils.get_config(:default, :client, OpenAPIClient)

    client.operation(
      %OpenAPIClient.State{
        request_base_url: base_url,
        request_path: "/api/request",
        method: :post,
        request_types: [{"application/json", {LiqPayAPI.P2PdebitTransfer.Request, :t}}],
        function_args: [body: body],
        function_call: {__MODULE__, :transfer},
        function_opts: opts,
        profile: :default
      },
      pipeline
    )
  end
end
