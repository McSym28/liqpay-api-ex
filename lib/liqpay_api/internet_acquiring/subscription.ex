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

    * `private_key`: Private key of the created company (not available to anyone except your developer). Default value obtained through a call to `Application.get_env(:liqpay_api_ex, :private_key)`
    * `base_url`: Request's base URL. Default value is taken from `@base_url`
    * `client_pipeline`: Client pipeline for making a request. Default value obtained through a call to `OpenAPIClient.Utils.get_config(__operation__, :client_pipeline)}

  """
  @spec create(LiqPayAPI.InternetAcquiring.Subscription.Create.Request.t(), [
          {:private_key, String.t()}
          | {:base_url, String.t() | URI.t()}
          | {:client_pipeline, OpenAPIClient.Client.pipeline()}
        ]) ::
          {:ok, LiqPayAPI.InternetAcquiring.Subscription.Create.Response.t()}
          | {:error, OpenAPIClient.Client.Error.t()}
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

    initial_args = [body: body]

    client_pipeline = Keyword.get(opts, :client_pipeline)
    base_url = opts[:base_url] || @base_url

    private_key =
      Keyword.get_lazy(opts, :private_key, fn ->
        Application.get_env(:liqpay_api_ex, :private_key)
      end)

    client = OpenAPIClient.Utils.get_config(:default, :client, OpenAPIClient.Client)

    %OpenAPIClient.Client.Operation{
      request_base_url: base_url,
      request_url: "/api/request",
      request_body: body,
      request_method: :post,
      request_types: [
        {"application/json", {LiqPayAPI.InternetAcquiring.Subscription.Create.Request, :t}}
      ],
      response_types: [
        {200,
         [{"application/json", {LiqPayAPI.InternetAcquiring.Subscription.Create.Response, :t}}]}
      ]
    }
    |> OpenAPIClient.Client.Operation.put_private(
      __args__: initial_args,
      __call__: {__MODULE__, :create},
      __opts__: opts,
      __params__: [private_key: private_key],
      __profile__: :default
    )
    |> client.perform(client_pipeline)
  end

  @doc """
  Internet acquiring. Subscription. Edit subscribtion

  ## Arguments

    * `body`

  ## Options

    * `private_key`: Private key of the created company (not available to anyone except your developer). Default value obtained through a call to `Application.get_env(:liqpay_api_ex, :private_key)`
    * `base_url`: Request's base URL. Default value is taken from `@base_url`
    * `client_pipeline`: Client pipeline for making a request. Default value obtained through a call to `OpenAPIClient.Utils.get_config(__operation__, :client_pipeline)}

  """
  @spec edit(LiqPayAPI.InternetAcquiring.Subscription.Edit.Request.t(), [
          {:private_key, String.t()}
          | {:base_url, String.t() | URI.t()}
          | {:client_pipeline, OpenAPIClient.Client.pipeline()}
        ]) ::
          {:ok, LiqPayAPI.InternetAcquiring.Subscription.Edit.Response.t()}
          | {:error, OpenAPIClient.Client.Error.t()}
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

    initial_args = [body: body]

    client_pipeline = Keyword.get(opts, :client_pipeline)
    base_url = opts[:base_url] || @base_url

    private_key =
      Keyword.get_lazy(opts, :private_key, fn ->
        Application.get_env(:liqpay_api_ex, :private_key)
      end)

    client = OpenAPIClient.Utils.get_config(:default, :client, OpenAPIClient.Client)

    %OpenAPIClient.Client.Operation{
      request_base_url: base_url,
      request_url: "/api/request",
      request_body: body,
      request_method: :post,
      request_types: [
        {"application/json", {LiqPayAPI.InternetAcquiring.Subscription.Edit.Request, :t}}
      ],
      response_types: [
        {200,
         [{"application/json", {LiqPayAPI.InternetAcquiring.Subscription.Edit.Response, :t}}]}
      ]
    }
    |> OpenAPIClient.Client.Operation.put_private(
      __args__: initial_args,
      __call__: {__MODULE__, :edit},
      __opts__: opts,
      __params__: [private_key: private_key],
      __profile__: :default
    )
    |> client.perform(client_pipeline)
  end

  @doc """
  Internet acquiring. Subscription. Unsubscribe

  ## Arguments

    * `body`

  ## Options

    * `private_key`: Private key of the created company (not available to anyone except your developer). Default value obtained through a call to `Application.get_env(:liqpay_api_ex, :private_key)`
    * `base_url`: Request's base URL. Default value is taken from `@base_url`
    * `client_pipeline`: Client pipeline for making a request. Default value obtained through a call to `OpenAPIClient.Utils.get_config(__operation__, :client_pipeline)}

  """
  @spec unsubscribe(LiqPayAPI.InternetAcquiring.Subscription.Unsubscribe.Request.t(), [
          {:private_key, String.t()}
          | {:base_url, String.t() | URI.t()}
          | {:client_pipeline, OpenAPIClient.Client.pipeline()}
        ]) ::
          {:ok, LiqPayAPI.InternetAcquiring.Subscription.Unsubscribe.Response.t()}
          | {:error, OpenAPIClient.Client.Error.t()}
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

    initial_args = [body: body]

    client_pipeline = Keyword.get(opts, :client_pipeline)
    base_url = opts[:base_url] || @base_url

    private_key =
      Keyword.get_lazy(opts, :private_key, fn ->
        Application.get_env(:liqpay_api_ex, :private_key)
      end)

    client = OpenAPIClient.Utils.get_config(:default, :client, OpenAPIClient.Client)

    %OpenAPIClient.Client.Operation{
      request_base_url: base_url,
      request_url: "/api/request",
      request_body: body,
      request_method: :post,
      request_types: [
        {"application/json", {LiqPayAPI.InternetAcquiring.Subscription.Unsubscribe.Request, :t}}
      ],
      response_types: [
        {200,
         [
           {"application/json",
            {LiqPayAPI.InternetAcquiring.Subscription.Unsubscribe.Response, :t}}
         ]}
      ]
    }
    |> OpenAPIClient.Client.Operation.put_private(
      __args__: initial_args,
      __call__: {__MODULE__, :unsubscribe},
      __opts__: opts,
      __params__: [private_key: private_key],
      __profile__: :default
    )
    |> client.perform(client_pipeline)
  end
end
