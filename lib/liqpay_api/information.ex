defmodule LiqPayAPI.Information do
  @moduledoc """
  Provides API endpoints related to information
  """

  @base_url "https://www.liqpay.ua"

  @doc """
  Informational. Adding data

  ## Arguments

    * `body`

  ## Options

    * `private_key`: Private key of the created company (not available to anyone except your developer). Default value obtained through a call to `Application.get_env(:liqpay_api_ex, :private_key)`
    * `base_url`: Request's base URL. Default value is taken from `@base_url`
    * `client_pipeline`: Client pipeline for making a request. Default value obtained through a call to `OpenAPIClient.Utils.get_config(__operation__, :client_pipeline)}

  """
  @spec adding_data(LiqPayAPI.Information.AddingData.Request.t(), [
          {:private_key, String.t()}
          | {:base_url, String.t() | URI.t()}
          | {:client_pipeline, OpenAPIClient.Client.pipeline()}
        ]) ::
          {:ok, LiqPayAPI.Information.AddingData.Response.t()}
          | {:error, OpenAPIClient.Client.Error.t()}
  def adding_data(body, opts \\ []) do
    body =
      case body do
        %LiqPayAPI.Information.AddingData.Request{public_key: nil} ->
          %LiqPayAPI.Information.AddingData.Request{
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
      request_types: [{"application/json", {LiqPayAPI.Information.AddingData.Request, :t}}],
      response_types: [
        {200, [{"application/json", {LiqPayAPI.Information.AddingData.Response, :t}}]}
      ]
    }
    |> OpenAPIClient.Client.Operation.put_private(
      __args__: initial_args,
      __call__: {__MODULE__, :adding_data},
      __opts__: opts,
      __params__: [private_key: private_key],
      __profile__: :default
    )
    |> client.perform(client_pipeline)
  end

  @doc """
  Informational. Payments archive

  ## Arguments

    * `body`

  ## Options

    * `private_key`: Private key of the created company (not available to anyone except your developer). Default value obtained through a call to `Application.get_env(:liqpay_api_ex, :private_key)`
    * `base_url`: Request's base URL. Default value is taken from `@base_url`
    * `client_pipeline`: Client pipeline for making a request. Default value obtained through a call to `OpenAPIClient.Utils.get_config(__operation__, :client_pipeline)}

  """
  @spec payment_archive(LiqPayAPI.Information.PaymentArchive.Request.t(), [
          {:private_key, String.t()}
          | {:base_url, String.t() | URI.t()}
          | {:client_pipeline, OpenAPIClient.Client.pipeline()}
        ]) ::
          {:ok, LiqPayAPI.Information.PaymentArchive.Response.t()}
          | {:error, OpenAPIClient.Client.Error.t()}
  def payment_archive(body, opts \\ []) do
    body =
      case body do
        %LiqPayAPI.Information.PaymentArchive.Request{public_key: nil} ->
          %LiqPayAPI.Information.PaymentArchive.Request{
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
      request_types: [{"application/json", {LiqPayAPI.Information.PaymentArchive.Request, :t}}],
      response_types: [
        {200, [{"application/json", {LiqPayAPI.Information.PaymentArchive.Response, :t}}]}
      ]
    }
    |> OpenAPIClient.Client.Operation.put_private(
      __args__: initial_args,
      __call__: {__MODULE__, :payment_archive},
      __opts__: opts,
      __params__: [private_key: private_key],
      __profile__: :default
    )
    |> client.perform(client_pipeline)
  end

  @doc """
  Informational. Receive a receipt

  ## Arguments

    * `body`

  ## Options

    * `private_key`: Private key of the created company (not available to anyone except your developer). Default value obtained through a call to `Application.get_env(:liqpay_api_ex, :private_key)`
    * `base_url`: Request's base URL. Default value is taken from `@base_url`
    * `client_pipeline`: Client pipeline for making a request. Default value obtained through a call to `OpenAPIClient.Utils.get_config(__operation__, :client_pipeline)}

  """
  @spec receive_receipt(LiqPayAPI.Information.ReceiveReceipt.Request.t(), [
          {:private_key, String.t()}
          | {:base_url, String.t() | URI.t()}
          | {:client_pipeline, OpenAPIClient.Client.pipeline()}
        ]) ::
          {:ok, LiqPayAPI.Information.ReceiveReceipt.Response.t()}
          | {:error, OpenAPIClient.Client.Error.t()}
  def receive_receipt(body, opts \\ []) do
    body =
      case body do
        %LiqPayAPI.Information.ReceiveReceipt.Request{public_key: nil} ->
          %LiqPayAPI.Information.ReceiveReceipt.Request{
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
      request_types: [{"application/json", {LiqPayAPI.Information.ReceiveReceipt.Request, :t}}],
      response_types: [
        {200, [{"application/json", {LiqPayAPI.Information.ReceiveReceipt.Response, :t}}]}
      ]
    }
    |> OpenAPIClient.Client.Operation.put_private(
      __args__: initial_args,
      __call__: {__MODULE__, :receive_receipt},
      __opts__: opts,
      __params__: [private_key: private_key],
      __profile__: :default
    )
    |> client.perform(client_pipeline)
  end

  @doc """
  Informational. Payment status

  ## Arguments

    * `body`

  ## Options

    * `private_key`: Private key of the created company (not available to anyone except your developer). Default value obtained through a call to `Application.get_env(:liqpay_api_ex, :private_key)`
    * `base_url`: Request's base URL. Default value is taken from `@base_url`
    * `client_pipeline`: Client pipeline for making a request. Default value obtained through a call to `OpenAPIClient.Utils.get_config(__operation__, :client_pipeline)}

  """
  @spec status_payment(LiqPayAPI.Information.StatusPayment.Request.t(), [
          {:private_key, String.t()}
          | {:base_url, String.t() | URI.t()}
          | {:client_pipeline, OpenAPIClient.Client.pipeline()}
        ]) ::
          {:ok, LiqPayAPI.Information.StatusPayment.Response.t()}
          | {:error, OpenAPIClient.Client.Error.t()}
  def status_payment(body, opts \\ []) do
    body =
      case body do
        %LiqPayAPI.Information.StatusPayment.Request{public_key: nil} ->
          %LiqPayAPI.Information.StatusPayment.Request{
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
      request_types: [{"application/json", {LiqPayAPI.Information.StatusPayment.Request, :t}}],
      response_types: [
        {200, [{"application/json", {LiqPayAPI.Information.StatusPayment.Response, :t}}]}
      ]
    }
    |> OpenAPIClient.Client.Operation.put_private(
      __args__: initial_args,
      __call__: {__MODULE__, :status_payment},
      __opts__: opts,
      __params__: [private_key: private_key],
      __profile__: :default
    )
    |> client.perform(client_pipeline)
  end
end
