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

    * `base_url`: Request's base URL. Default value is taken from `@base_url`
    * `pipeline`: Operation pipeline for making a request. Default value obtained through a call to `OpenAPIClient.Utils.get_config(:default, :operation_pipeline)}
    * `client`: Module that implements `OpenAPIClient` behaviour. Default value obtained through a call to `OpenAPIClient.Utils.get_config(:default, :client, OpenAPIClient)`

  """
  @spec adding_data(LiqPayAPI.Information.AddingData.Request.t()) ::
          {:ok, LiqPayAPI.Information.AddingData.Response.t()} | {:error, OpenAPIClient.Error.t()}
  @spec adding_data(LiqPayAPI.Information.AddingData.Request.t(), [
          {:base_url, String.t() | URI.t()}
          | {:pipeline, OpenAPIClient.pipeline()}
          | {:client, module()}
        ]) ::
          {:ok, LiqPayAPI.Information.AddingData.Response.t()} | {:error, OpenAPIClient.Error.t()}
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

    pipeline = opts[:pipeline] || OpenAPIClient.Utils.get_config(:default, :operation_pipeline)
    base_url = opts[:base_url] || @base_url
    client = opts[:client] || OpenAPIClient.Utils.get_config(:default, :client, OpenAPIClient)

    client.operation(
      %OpenAPIClient.State{
        request_base_url: base_url,
        request_path: "/api/request?path=information.adding_data",
        method: :post,
        request_types: [{"application/json", {LiqPayAPI.Information.AddingData.Request, :t}}],
        response_types: [
          {200, [{"application/json", {LiqPayAPI.Information.AddingData.Response, :t}}]}
        ],
        function_args: [body: body],
        function_call: {__MODULE__, :adding_data},
        function_opts: opts,
        profile: :default
      },
      pipeline
    )
  end

  @doc """
  Informational. Payments archive

  ## Arguments

    * `body`

  ## Options

    * `base_url`: Request's base URL. Default value is taken from `@base_url`
    * `pipeline`: Operation pipeline for making a request. Default value obtained through a call to `OpenAPIClient.Utils.get_config(:default, :operation_pipeline)}
    * `client`: Module that implements `OpenAPIClient` behaviour. Default value obtained through a call to `OpenAPIClient.Utils.get_config(:default, :client, OpenAPIClient)`

  """
  @spec payment_archive(LiqPayAPI.Information.PaymentArchive.Request.t()) ::
          {:ok, LiqPayAPI.Information.PaymentArchive.Response.t()}
          | {:error, OpenAPIClient.Error.t()}
  @spec payment_archive(LiqPayAPI.Information.PaymentArchive.Request.t(), [
          {:base_url, String.t() | URI.t()}
          | {:pipeline, OpenAPIClient.pipeline()}
          | {:client, module()}
        ]) ::
          {:ok, LiqPayAPI.Information.PaymentArchive.Response.t()}
          | {:error, OpenAPIClient.Error.t()}
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

    pipeline = opts[:pipeline] || OpenAPIClient.Utils.get_config(:default, :operation_pipeline)
    base_url = opts[:base_url] || @base_url
    client = opts[:client] || OpenAPIClient.Utils.get_config(:default, :client, OpenAPIClient)

    client.operation(
      %OpenAPIClient.State{
        request_base_url: base_url,
        request_path: "/api/request?path=information.payment_archive",
        method: :post,
        request_types: [{"application/json", {LiqPayAPI.Information.PaymentArchive.Request, :t}}],
        response_types: [
          {200, [{"application/json", {LiqPayAPI.Information.PaymentArchive.Response, :t}}]}
        ],
        function_args: [body: body],
        function_call: {__MODULE__, :payment_archive},
        function_opts: opts,
        profile: :default
      },
      pipeline
    )
  end

  @doc """
  Informational. Receive a receipt

  ## Arguments

    * `body`

  ## Options

    * `base_url`: Request's base URL. Default value is taken from `@base_url`
    * `pipeline`: Operation pipeline for making a request. Default value obtained through a call to `OpenAPIClient.Utils.get_config(:default, :operation_pipeline)}
    * `client`: Module that implements `OpenAPIClient` behaviour. Default value obtained through a call to `OpenAPIClient.Utils.get_config(:default, :client, OpenAPIClient)`

  """
  @spec receive_receipt(LiqPayAPI.Information.ReceiveReceipt.Request.t()) ::
          {:ok, LiqPayAPI.Information.ReceiveReceipt.Response.t()}
          | {:error, OpenAPIClient.Error.t()}
  @spec receive_receipt(LiqPayAPI.Information.ReceiveReceipt.Request.t(), [
          {:base_url, String.t() | URI.t()}
          | {:pipeline, OpenAPIClient.pipeline()}
          | {:client, module()}
        ]) ::
          {:ok, LiqPayAPI.Information.ReceiveReceipt.Response.t()}
          | {:error, OpenAPIClient.Error.t()}
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

    pipeline = opts[:pipeline] || OpenAPIClient.Utils.get_config(:default, :operation_pipeline)
    base_url = opts[:base_url] || @base_url
    client = opts[:client] || OpenAPIClient.Utils.get_config(:default, :client, OpenAPIClient)

    client.operation(
      %OpenAPIClient.State{
        request_base_url: base_url,
        request_path: "/api/request?path=information.receive_a_receipt",
        method: :post,
        request_types: [{"application/json", {LiqPayAPI.Information.ReceiveReceipt.Request, :t}}],
        response_types: [
          {200, [{"application/json", {LiqPayAPI.Information.ReceiveReceipt.Response, :t}}]}
        ],
        function_args: [body: body],
        function_call: {__MODULE__, :receive_receipt},
        function_opts: opts,
        profile: :default
      },
      pipeline
    )
  end

  @doc """
  Informational. Payment status

  ## Arguments

    * `body`

  ## Options

    * `base_url`: Request's base URL. Default value is taken from `@base_url`
    * `pipeline`: Operation pipeline for making a request. Default value obtained through a call to `OpenAPIClient.Utils.get_config(:default, :operation_pipeline)}
    * `client`: Module that implements `OpenAPIClient` behaviour. Default value obtained through a call to `OpenAPIClient.Utils.get_config(:default, :client, OpenAPIClient)`

  """
  @spec status_payment(LiqPayAPI.Information.StatusPayment.Request.t()) ::
          {:ok, LiqPayAPI.Information.StatusPayment.Response.t()}
          | {:error, OpenAPIClient.Error.t()}
  @spec status_payment(LiqPayAPI.Information.StatusPayment.Request.t(), [
          {:base_url, String.t() | URI.t()}
          | {:pipeline, OpenAPIClient.pipeline()}
          | {:client, module()}
        ]) ::
          {:ok, LiqPayAPI.Information.StatusPayment.Response.t()}
          | {:error, OpenAPIClient.Error.t()}
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

    pipeline = opts[:pipeline] || OpenAPIClient.Utils.get_config(:default, :operation_pipeline)
    base_url = opts[:base_url] || @base_url
    client = opts[:client] || OpenAPIClient.Utils.get_config(:default, :client, OpenAPIClient)

    client.operation(
      %OpenAPIClient.State{
        request_base_url: base_url,
        request_path: "/api/request?path=information.status_payment",
        method: :post,
        request_types: [{"application/json", {LiqPayAPI.Information.StatusPayment.Request, :t}}],
        response_types: [
          {200, [{"application/json", {LiqPayAPI.Information.StatusPayment.Response, :t}}]}
        ],
        function_args: [body: body],
        function_call: {__MODULE__, :status_payment},
        function_opts: opts,
        profile: :default
      },
      pipeline
    )
  end
end
