defmodule LiqPayAPI.InternetAcquiring.Qr do
  @moduledoc """
  Provides API endpoints related to internet acquiring/qr
  """

  @base_url "https://www.liqpay.ua"

  @doc """
  Internet acquiring. QR-code payment. Dynamic QR-code

  ## Arguments

    * `body`

  ## Options

    * `private_key`: Private key of the created company (not available to anyone except your developer). Default value obtained through a call to `Application.get_env(:liqpay_api_ex, :private_key)`
    * `base_url`: Request's base URL. Default value is taken from `@base_url`
    * `pipeline`: Operation pipeline for making a request. Default value obtained through a call to `OpenAPIClient.Utils.get_config(:default, :operation_pipeline)}
    * `client`: Module that implements `OpenAPIClient` behaviour. Default value obtained through a call to `OpenAPIClient.Utils.get_config(:default, :client, OpenAPIClient)`

  """
  @spec dynamic(LiqPayAPI.InternetAcquiring.QR.Dynamic.Request.t()) ::
          {:ok, LiqPayAPI.InternetAcquiring.QR.Dynamic.Response.t()}
          | {:error, OpenAPIClient.Error.t()}
  @spec dynamic(LiqPayAPI.InternetAcquiring.QR.Dynamic.Request.t(), [
          {:private_key, String.t()}
          | {:base_url, String.t() | URI.t()}
          | {:pipeline, OpenAPIClient.pipeline()}
          | {:client, module()}
        ]) ::
          {:ok, LiqPayAPI.InternetAcquiring.QR.Dynamic.Response.t()}
          | {:error, OpenAPIClient.Error.t()}
  def dynamic(body, opts \\ []) do
    body =
      case body do
        %LiqPayAPI.InternetAcquiring.QR.Dynamic.Request{public_key: nil} ->
          %LiqPayAPI.InternetAcquiring.QR.Dynamic.Request{
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
        request_types: [
          {"application/json", {LiqPayAPI.InternetAcquiring.QR.Dynamic.Request, :t}}
        ],
        response_types: [
          {200, [{"application/json", {LiqPayAPI.InternetAcquiring.QR.Dynamic.Response, :t}}]}
        ],
        function_args: [body: body],
        function_call: {__MODULE__, :dynamic},
        function_opts: opts,
        profile: :default
      },
      pipeline
    )
  end

  @doc """
  Internet acquiring. QR-code payment. Static QR-code

  ## Arguments

    * `body`

  ## Options

    * `private_key`: Private key of the created company (not available to anyone except your developer). Default value obtained through a call to `Application.get_env(:liqpay_api_ex, :private_key)`
    * `base_url`: Request's base URL. Default value is taken from `@base_url`
    * `pipeline`: Operation pipeline for making a request. Default value obtained through a call to `OpenAPIClient.Utils.get_config(:default, :operation_pipeline)}
    * `client`: Module that implements `OpenAPIClient` behaviour. Default value obtained through a call to `OpenAPIClient.Utils.get_config(:default, :client, OpenAPIClient)`

  """
  @spec static(LiqPayAPI.InternetAcquiring.QR.Static.Request.t()) ::
          {:ok, LiqPayAPI.InternetAcquiring.QR.Static.Response.t()}
          | {:error, OpenAPIClient.Error.t()}
  @spec static(LiqPayAPI.InternetAcquiring.QR.Static.Request.t(), [
          {:private_key, String.t()}
          | {:base_url, String.t() | URI.t()}
          | {:pipeline, OpenAPIClient.pipeline()}
          | {:client, module()}
        ]) ::
          {:ok, LiqPayAPI.InternetAcquiring.QR.Static.Response.t()}
          | {:error, OpenAPIClient.Error.t()}
  def static(body, opts \\ []) do
    body =
      case body do
        %LiqPayAPI.InternetAcquiring.QR.Static.Request{public_key: nil} ->
          %LiqPayAPI.InternetAcquiring.QR.Static.Request{
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
        request_types: [{"application/json", {LiqPayAPI.InternetAcquiring.QR.Static.Request, :t}}],
        response_types: [
          {200, [{"application/json", {LiqPayAPI.InternetAcquiring.QR.Static.Response, :t}}]}
        ],
        function_args: [body: body],
        function_call: {__MODULE__, :static},
        function_opts: opts,
        profile: :default
      },
      pipeline
    )
  end
end
