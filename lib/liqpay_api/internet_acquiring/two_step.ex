defmodule LiqPayAPI.InternetAcquiring.TwoStep do
  @moduledoc """
  Provides API endpoints related to internet acquiring/two step
  """

  @base_url "https://www.liqpay.ua"

  @doc """
  Internet acquiring. Two-step payment. Funds blocking

  ## Arguments

    * `body`

  ## Options

    * `base_url`: Request's base URL. Default value is taken from `@base_url`
    * `pipeline`: Operation pipeline for making a request. Default value obtained through a call to `OpenAPIClient.Utils.get_config(:default, :operation_pipeline)}
    * `client`: Module that implements `OpenAPIClient` behaviour. Default value obtained through a call to `OpenAPIClient.Utils.get_config(:default, :client, OpenAPIClient)`

  """
  @spec block(LiqPayAPI.InternetAcquiring.TwoStep.Block.Request.t()) ::
          {:ok, LiqPayAPI.InternetAcquiring.TwoStep.Block.Response.t()}
          | {:error, OpenAPIClient.Error.t()}
  @spec block(LiqPayAPI.InternetAcquiring.TwoStep.Block.Request.t(), [
          {:base_url, String.t() | URI.t()}
          | {:pipeline, OpenAPIClient.pipeline()}
          | {:client, module()}
        ]) ::
          {:ok, LiqPayAPI.InternetAcquiring.TwoStep.Block.Response.t()}
          | {:error, OpenAPIClient.Error.t()}
  def block(body, opts \\ []) do
    body =
      case body do
        %LiqPayAPI.InternetAcquiring.TwoStep.Block.Request{public_key: nil} ->
          %LiqPayAPI.InternetAcquiring.TwoStep.Block.Request{
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
        request_path: "/api/request?path=internet_acquiring.two_step.block",
        method: :post,
        request_types: [
          {"application/json", {LiqPayAPI.InternetAcquiring.TwoStep.Block.Request, :t}}
        ],
        response_types: [
          {200, [{"application/json", {LiqPayAPI.InternetAcquiring.TwoStep.Block.Response, :t}}]}
        ],
        function_args: [body: body],
        function_call: {__MODULE__, :block},
        function_opts: opts,
        profile: :default
      },
      pipeline
    )
  end

  @doc """
  Internet acquiring. Two-step payment. Completion of payment

  ## Arguments

    * `body`

  ## Options

    * `base_url`: Request's base URL. Default value is taken from `@base_url`
    * `pipeline`: Operation pipeline for making a request. Default value obtained through a call to `OpenAPIClient.Utils.get_config(:default, :operation_pipeline)}
    * `client`: Module that implements `OpenAPIClient` behaviour. Default value obtained through a call to `OpenAPIClient.Utils.get_config(:default, :client, OpenAPIClient)`

  """
  @spec complete(LiqPayAPI.InternetAcquiring.TwoStep.Complete.Request.t()) ::
          {:ok, LiqPayAPI.InternetAcquiring.TwoStep.Complete.Response.t()}
          | {:error, OpenAPIClient.Error.t()}
  @spec complete(LiqPayAPI.InternetAcquiring.TwoStep.Complete.Request.t(), [
          {:base_url, String.t() | URI.t()}
          | {:pipeline, OpenAPIClient.pipeline()}
          | {:client, module()}
        ]) ::
          {:ok, LiqPayAPI.InternetAcquiring.TwoStep.Complete.Response.t()}
          | {:error, OpenAPIClient.Error.t()}
  def complete(body, opts \\ []) do
    body =
      case body do
        %LiqPayAPI.InternetAcquiring.TwoStep.Complete.Request{public_key: nil} ->
          %LiqPayAPI.InternetAcquiring.TwoStep.Complete.Request{
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
        request_path: "/api/request?path=internet_acquiring.two_step.complete",
        method: :post,
        request_types: [
          {"application/json", {LiqPayAPI.InternetAcquiring.TwoStep.Complete.Request, :t}}
        ],
        response_types: [
          {200,
           [{"application/json", {LiqPayAPI.InternetAcquiring.TwoStep.Complete.Response, :t}}]}
        ],
        function_args: [body: body],
        function_call: {__MODULE__, :complete},
        function_opts: opts,
        profile: :default
      },
      pipeline
    )
  end
end
