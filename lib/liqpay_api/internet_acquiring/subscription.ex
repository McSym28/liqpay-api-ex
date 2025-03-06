defmodule LiqPayAPI.InternetAcquiring.Subscription do
  @moduledoc """
  Provides API endpoints related to internet acquiring/subscription
  """

  @base_url "https://www.liqpay.ua"

  @doc """
  Internet acquiring. Subscription. Create subscribtion

  ## Arguments

    * `body`

  ## Options

    * `base_url`: Request's base URL. Default value is taken from `@base_url`
    * `pipeline`: Operation pipeline for making a request. Default value obtained through a call to `OpenAPIClient.Utils.get_config(:default, :operation_pipeline)}
    * `client`: Module that implements `OpenAPIClient` behaviour. Default value obtained through a call to `OpenAPIClient.Utils.get_config(:default, :client, OpenAPIClient)`

  """
  @spec create(LiqPayAPI.InternetAcquiring.Subscription.Create.Request.t()) ::
          {:ok, LiqPayAPI.InternetAcquiring.Subscription.Create.Response.t()}
          | {:error, OpenAPIClient.Error.t()}
  @spec create(LiqPayAPI.InternetAcquiring.Subscription.Create.Request.t(), [
          {:base_url, String.t() | URI.t()}
          | {:pipeline, OpenAPIClient.pipeline()}
          | {:client, module()}
        ]) ::
          {:ok, LiqPayAPI.InternetAcquiring.Subscription.Create.Response.t()}
          | {:error, OpenAPIClient.Error.t()}
  def create(body, opts \\ []) do
    body =
      case body do
        %LiqPayAPI.InternetAcquiring.Subscription.Create.Request{public_key: nil} ->
          %LiqPayAPI.InternetAcquiring.Subscription.Create.Request{
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
        request_path: "/api/request?path=internet_acquiring.subscription.create",
        method: :post,
        request_types: [
          {"application/json", {LiqPayAPI.InternetAcquiring.Subscription.Create.Request, :t}}
        ],
        response_types: [
          {200,
           [{"application/json", {LiqPayAPI.InternetAcquiring.Subscription.Create.Response, :t}}]}
        ],
        function_args: [body: body],
        function_call: {__MODULE__, :create},
        function_opts: opts,
        profile: :default
      },
      pipeline
    )
  end

  @doc """
  Internet acquiring. Subscription. Edit subscribtion

  ## Arguments

    * `body`

  ## Options

    * `base_url`: Request's base URL. Default value is taken from `@base_url`
    * `pipeline`: Operation pipeline for making a request. Default value obtained through a call to `OpenAPIClient.Utils.get_config(:default, :operation_pipeline)}
    * `client`: Module that implements `OpenAPIClient` behaviour. Default value obtained through a call to `OpenAPIClient.Utils.get_config(:default, :client, OpenAPIClient)`

  """
  @spec edit(LiqPayAPI.InternetAcquiring.Subscription.Edit.Request.t()) ::
          {:ok, LiqPayAPI.InternetAcquiring.Subscription.Edit.Response.t()}
          | {:error, OpenAPIClient.Error.t()}
  @spec edit(LiqPayAPI.InternetAcquiring.Subscription.Edit.Request.t(), [
          {:base_url, String.t() | URI.t()}
          | {:pipeline, OpenAPIClient.pipeline()}
          | {:client, module()}
        ]) ::
          {:ok, LiqPayAPI.InternetAcquiring.Subscription.Edit.Response.t()}
          | {:error, OpenAPIClient.Error.t()}
  def edit(body, opts \\ []) do
    body =
      case body do
        %LiqPayAPI.InternetAcquiring.Subscription.Edit.Request{public_key: nil} ->
          %LiqPayAPI.InternetAcquiring.Subscription.Edit.Request{
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
        request_path: "/api/request?path=internet_acquiring.subscription.edit",
        method: :post,
        request_types: [
          {"application/json", {LiqPayAPI.InternetAcquiring.Subscription.Edit.Request, :t}}
        ],
        response_types: [
          {200,
           [{"application/json", {LiqPayAPI.InternetAcquiring.Subscription.Edit.Response, :t}}]}
        ],
        function_args: [body: body],
        function_call: {__MODULE__, :edit},
        function_opts: opts,
        profile: :default
      },
      pipeline
    )
  end

  @doc """
  Internet acquiring. Subscription. Unsubscribe

  ## Arguments

    * `body`

  ## Options

    * `base_url`: Request's base URL. Default value is taken from `@base_url`
    * `pipeline`: Operation pipeline for making a request. Default value obtained through a call to `OpenAPIClient.Utils.get_config(:default, :operation_pipeline)}
    * `client`: Module that implements `OpenAPIClient` behaviour. Default value obtained through a call to `OpenAPIClient.Utils.get_config(:default, :client, OpenAPIClient)`

  """
  @spec unsubscribe(LiqPayAPI.InternetAcquiring.Subscription.Unsubscribe.Request.t()) ::
          {:ok, LiqPayAPI.InternetAcquiring.Subscription.Unsubscribe.Response.t()}
          | {:error, OpenAPIClient.Error.t()}
  @spec unsubscribe(LiqPayAPI.InternetAcquiring.Subscription.Unsubscribe.Request.t(), [
          {:base_url, String.t() | URI.t()}
          | {:pipeline, OpenAPIClient.pipeline()}
          | {:client, module()}
        ]) ::
          {:ok, LiqPayAPI.InternetAcquiring.Subscription.Unsubscribe.Response.t()}
          | {:error, OpenAPIClient.Error.t()}
  def unsubscribe(body, opts \\ []) do
    body =
      case body do
        %LiqPayAPI.InternetAcquiring.Subscription.Unsubscribe.Request{public_key: nil} ->
          %LiqPayAPI.InternetAcquiring.Subscription.Unsubscribe.Request{
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
        request_path: "/api/request?path=internet_acquiring.subscription.unsubscribe",
        method: :post,
        request_types: [
          {"application/json", {LiqPayAPI.InternetAcquiring.Subscription.Unsubscribe.Request, :t}}
        ],
        response_types: [
          {200,
           [
             {"application/json",
              {LiqPayAPI.InternetAcquiring.Subscription.Unsubscribe.Response, :t}}
           ]}
        ],
        function_args: [body: body],
        function_call: {__MODULE__, :unsubscribe},
        function_opts: opts,
        profile: :default
      },
      pipeline
    )
  end
end
