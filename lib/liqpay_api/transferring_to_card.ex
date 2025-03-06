defmodule LiqPayAPI.TransferringToCard do
  @moduledoc """
  Provides API endpoint related to transferring to card
  """

  @base_url "https://www.liqpay.ua"

  @doc """
  Transfer of funds from account to card

  ## Arguments

    * `body`

  ## Options

    * `private_key`: Private key of the created company (not available to anyone except your developer). Default value obtained through a call to `Application.get_env(:liqpay_api_ex, :private_key)`
    * `base_url`: Request's base URL. Default value is taken from `@base_url`
    * `pipeline`: Operation pipeline for making a request. Default value obtained through a call to `OpenAPIClient.Utils.get_config(:default, :operation_pipeline)}
    * `client`: Module that implements `OpenAPIClient` behaviour. Default value obtained through a call to `OpenAPIClient.Utils.get_config(:default, :client, OpenAPIClient)`

  """
  @spec transfer(LiqPayAPI.TransferringToCard.Transfer.Request.t()) ::
          {:ok, LiqPayAPI.TransferringToCard.Transfer.Response.t()}
          | {:error, OpenAPIClient.Error.t()}
  @spec transfer(LiqPayAPI.TransferringToCard.Transfer.Request.t(), [
          {:private_key, String.t()}
          | {:base_url, String.t() | URI.t()}
          | {:pipeline, OpenAPIClient.pipeline()}
          | {:client, module()}
        ]) ::
          {:ok, LiqPayAPI.TransferringToCard.Transfer.Response.t()}
          | {:error, OpenAPIClient.Error.t()}
  def transfer(body, opts \\ []) do
    body =
      case body do
        %LiqPayAPI.TransferringToCard.Transfer.Request{public_key: nil} ->
          %LiqPayAPI.TransferringToCard.Transfer.Request{
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
        request_parameter_types: [
          {{:private_key, :custom},
           {"private_key", {:string, :generic},
            fn -> Application.get_env(:liqpay_api_ex, :private_key) end}}
        ],
        request_types: [{"application/json", {LiqPayAPI.TransferringToCard.Transfer.Request, :t}}],
        response_types: [
          {200, [{"application/json", {LiqPayAPI.TransferringToCard.Transfer.Response, :t}}]}
        ],
        function_args: [body: body],
        function_call: {__MODULE__, :transfer},
        function_opts: opts,
        profile: :default
      },
      pipeline
    )
  end
end
