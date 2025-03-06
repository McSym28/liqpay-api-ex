defmodule LiqPayAPI.InternetAcquiring do
  @moduledoc """
  Provides API endpoints related to internet acquiring
  """

  @base_url "https://www.liqpay.ua"

  @doc """
  Internet acquiring. Card payment

  ## Arguments

    * `body`

  ## Options

    * `base_url`: Request's base URL. Default value is taken from `@base_url`
    * `pipeline`: Operation pipeline for making a request. Default value obtained through a call to `OpenAPIClient.Utils.get_config(:default, :operation_pipeline)}
    * `client`: Module that implements `OpenAPIClient` behaviour. Default value obtained through a call to `OpenAPIClient.Utils.get_config(:default, :client, OpenAPIClient)`

  """
  @spec card_payment(LiqPayAPI.InternetAcquiring.CardPayment.Request.t()) ::
          {:ok, LiqPayAPI.InternetAcquiring.CardPayment.Response.t()}
          | {:error, OpenAPIClient.Error.t()}
  @spec card_payment(LiqPayAPI.InternetAcquiring.CardPayment.Request.t(), [
          {:base_url, String.t() | URI.t()}
          | {:pipeline, OpenAPIClient.pipeline()}
          | {:client, module()}
        ]) ::
          {:ok, LiqPayAPI.InternetAcquiring.CardPayment.Response.t()}
          | {:error, OpenAPIClient.Error.t()}
  def card_payment(body, opts \\ []) do
    body =
      case body do
        %LiqPayAPI.InternetAcquiring.CardPayment.Request{public_key: nil} ->
          %LiqPayAPI.InternetAcquiring.CardPayment.Request{
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
        request_types: [
          {"application/json", {LiqPayAPI.InternetAcquiring.CardPayment.Request, :t}}
        ],
        response_types: [
          {200, [{"application/json", {LiqPayAPI.InternetAcquiring.CardPayment.Response, :t}}]}
        ],
        function_args: [body: body],
        function_call: {__MODULE__, :card_payment},
        function_opts: opts,
        profile: :default
      },
      pipeline
    )
  end

  @doc """
  Internet acquiring. Cash payment

  ## Arguments

    * `body`

  ## Options

    * `base_url`: Request's base URL. Default value is taken from `@base_url`
    * `pipeline`: Operation pipeline for making a request. Default value obtained through a call to `OpenAPIClient.Utils.get_config(:default, :operation_pipeline)}
    * `client`: Module that implements `OpenAPIClient` behaviour. Default value obtained through a call to `OpenAPIClient.Utils.get_config(:default, :client, OpenAPIClient)`

  """
  @spec cash(LiqPayAPI.InternetAcquiring.Cash.Request.t()) ::
          {:ok, LiqPayAPI.InternetAcquiring.Cash.Response.t()} | {:error, OpenAPIClient.Error.t()}
  @spec cash(LiqPayAPI.InternetAcquiring.Cash.Request.t(), [
          {:base_url, String.t() | URI.t()}
          | {:pipeline, OpenAPIClient.pipeline()}
          | {:client, module()}
        ]) ::
          {:ok, LiqPayAPI.InternetAcquiring.Cash.Response.t()} | {:error, OpenAPIClient.Error.t()}
  def cash(body, opts \\ []) do
    body =
      case body do
        %LiqPayAPI.InternetAcquiring.Cash.Request{public_key: nil} ->
          %LiqPayAPI.InternetAcquiring.Cash.Request{
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
        request_types: [{"application/json", {LiqPayAPI.InternetAcquiring.Cash.Request, :t}}],
        response_types: [
          {200, [{"application/json", {LiqPayAPI.InternetAcquiring.Cash.Response, :t}}]}
        ],
        function_args: [body: body],
        function_call: {__MODULE__, :cash},
        function_opts: opts,
        profile: :default
      },
      pipeline
    )
  end

  @doc """
  Internet acquiring. Checkout

  ## Arguments

    * `body`

  ## Options

    * `base_url`: Request's base URL. Default value is taken from `@base_url`
    * `pipeline`: Operation pipeline for making a request. Default value obtained through a call to `OpenAPIClient.Utils.get_config(:default, :operation_pipeline)}
    * `client`: Module that implements `OpenAPIClient` behaviour. Default value obtained through a call to `OpenAPIClient.Utils.get_config(:default, :client, OpenAPIClient)`

  """
  @spec checkout(LiqPayAPI.InternetAcquiring.Checkout.Request.t()) ::
          :ok | {:error, OpenAPIClient.Error.t()}
  @spec checkout(LiqPayAPI.InternetAcquiring.Checkout.Request.t(), [
          {:base_url, String.t() | URI.t()}
          | {:pipeline, OpenAPIClient.pipeline()}
          | {:client, module()}
        ]) :: :ok | {:error, OpenAPIClient.Error.t()}
  def checkout(body, opts \\ []) do
    body =
      case body do
        %LiqPayAPI.InternetAcquiring.Checkout.Request{public_key: nil} ->
          %LiqPayAPI.InternetAcquiring.Checkout.Request{
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
        request_types: [{"application/json", {LiqPayAPI.InternetAcquiring.Checkout.Request, :t}}],
        function_args: [body: body],
        function_call: {__MODULE__, :checkout},
        function_opts: opts,
        profile: :default
      },
      pipeline
    )
  end

  @doc """
  Internet acquiring. DCC

  ## Arguments

    * `body`

  ## Options

    * `base_url`: Request's base URL. Default value is taken from `@base_url`
    * `pipeline`: Operation pipeline for making a request. Default value obtained through a call to `OpenAPIClient.Utils.get_config(:default, :operation_pipeline)}
    * `client`: Module that implements `OpenAPIClient` behaviour. Default value obtained through a call to `OpenAPIClient.Utils.get_config(:default, :client, OpenAPIClient)`

  """
  @spec dcc(LiqPayAPI.InternetAcquiring.DCC.Request.t()) ::
          {:ok, LiqPayAPI.InternetAcquiring.DCC.Response.t()} | {:error, OpenAPIClient.Error.t()}
  @spec dcc(LiqPayAPI.InternetAcquiring.DCC.Request.t(), [
          {:base_url, String.t() | URI.t()}
          | {:pipeline, OpenAPIClient.pipeline()}
          | {:client, module()}
        ]) ::
          {:ok, LiqPayAPI.InternetAcquiring.DCC.Response.t()} | {:error, OpenAPIClient.Error.t()}
  def dcc(body, opts \\ []) do
    body =
      case body do
        %LiqPayAPI.InternetAcquiring.DCC.Request{public_key: nil} ->
          %LiqPayAPI.InternetAcquiring.DCC.Request{
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
        request_types: [{"application/json", {LiqPayAPI.InternetAcquiring.DCC.Request, :t}}],
        response_types: [
          {200, [{"application/json", {LiqPayAPI.InternetAcquiring.DCC.Response, :t}}]}
        ],
        function_args: [body: body],
        function_call: {__MODULE__, :dcc},
        function_opts: opts,
        profile: :default
      },
      pipeline
    )
  end

  @doc """
  Internet acquiring. PrivatPay button

  ## Arguments

    * `body`

  ## Options

    * `base_url`: Request's base URL. Default value is taken from `@base_url`
    * `pipeline`: Operation pipeline for making a request. Default value obtained through a call to `OpenAPIClient.Utils.get_config(:default, :operation_pipeline)}
    * `client`: Module that implements `OpenAPIClient` behaviour. Default value obtained through a call to `OpenAPIClient.Utils.get_config(:default, :client, OpenAPIClient)`

  """
  @spec privat_pay(LiqPayAPI.InternetAcquiring.PrivatPay.Request.t()) ::
          :ok | {:error, OpenAPIClient.Error.t()}
  @spec privat_pay(LiqPayAPI.InternetAcquiring.PrivatPay.Request.t(), [
          {:base_url, String.t() | URI.t()}
          | {:pipeline, OpenAPIClient.pipeline()}
          | {:client, module()}
        ]) :: :ok | {:error, OpenAPIClient.Error.t()}
  def privat_pay(body, opts \\ []) do
    body =
      case body do
        %LiqPayAPI.InternetAcquiring.PrivatPay.Request{public_key: nil} ->
          %LiqPayAPI.InternetAcquiring.PrivatPay.Request{
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
        request_types: [{"application/json", {LiqPayAPI.InternetAcquiring.PrivatPay.Request, :t}}],
        function_args: [body: body],
        function_call: {__MODULE__, :privat_pay},
        function_opts: opts,
        profile: :default
      },
      pipeline
    )
  end

  @doc """
  Internet acquiring. QR-code payment

  ## Arguments

    * `body`

  ## Options

    * `base_url`: Request's base URL. Default value is taken from `@base_url`
    * `pipeline`: Operation pipeline for making a request. Default value obtained through a call to `OpenAPIClient.Utils.get_config(:default, :operation_pipeline)}
    * `client`: Module that implements `OpenAPIClient` behaviour. Default value obtained through a call to `OpenAPIClient.Utils.get_config(:default, :client, OpenAPIClient)`

  """
  @spec qr(LiqPayAPI.InternetAcquiring.QR.Request.t()) ::
          {:ok, LiqPayAPI.InternetAcquiring.QR.Response.t()} | {:error, OpenAPIClient.Error.t()}
  @spec qr(LiqPayAPI.InternetAcquiring.QR.Request.t(), [
          {:base_url, String.t() | URI.t()}
          | {:pipeline, OpenAPIClient.pipeline()}
          | {:client, module()}
        ]) ::
          {:ok, LiqPayAPI.InternetAcquiring.QR.Response.t()} | {:error, OpenAPIClient.Error.t()}
  def qr(body, opts \\ []) do
    body =
      case body do
        %LiqPayAPI.InternetAcquiring.QR.Request{public_key: nil} ->
          %LiqPayAPI.InternetAcquiring.QR.Request{
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
        request_types: [{"application/json", {LiqPayAPI.InternetAcquiring.QR.Request, :t}}],
        response_types: [
          {200, [{"application/json", {LiqPayAPI.InternetAcquiring.QR.Response, :t}}]}
        ],
        function_args: [body: body],
        function_call: {__MODULE__, :qr},
        function_opts: opts,
        profile: :default
      },
      pipeline
    )
  end

  @doc """
  Internet acquiring. Refund

  ## Arguments

    * `body`

  ## Options

    * `base_url`: Request's base URL. Default value is taken from `@base_url`
    * `pipeline`: Operation pipeline for making a request. Default value obtained through a call to `OpenAPIClient.Utils.get_config(:default, :operation_pipeline)}
    * `client`: Module that implements `OpenAPIClient` behaviour. Default value obtained through a call to `OpenAPIClient.Utils.get_config(:default, :client, OpenAPIClient)`

  """
  @spec refund(LiqPayAPI.InternetAcquiring.Refund.Request.t()) ::
          {:ok, LiqPayAPI.InternetAcquiring.Refund.Response.t()}
          | {:error, OpenAPIClient.Error.t()}
  @spec refund(LiqPayAPI.InternetAcquiring.Refund.Request.t(), [
          {:base_url, String.t() | URI.t()}
          | {:pipeline, OpenAPIClient.pipeline()}
          | {:client, module()}
        ]) ::
          {:ok, LiqPayAPI.InternetAcquiring.Refund.Response.t()}
          | {:error, OpenAPIClient.Error.t()}
  def refund(body, opts \\ []) do
    body =
      case body do
        %LiqPayAPI.InternetAcquiring.Refund.Request{public_key: nil} ->
          %LiqPayAPI.InternetAcquiring.Refund.Request{
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
        request_types: [{"application/json", {LiqPayAPI.InternetAcquiring.Refund.Request, :t}}],
        response_types: [
          {200, [{"application/json", {LiqPayAPI.InternetAcquiring.Refund.Response, :t}}]}
        ],
        function_args: [body: body],
        function_call: {__MODULE__, :refund},
        function_opts: opts,
        profile: :default
      },
      pipeline
    )
  end

  @doc """
  Internet acquiring. Token payment

  ## Arguments

    * `body`

  ## Options

    * `base_url`: Request's base URL. Default value is taken from `@base_url`
    * `pipeline`: Operation pipeline for making a request. Default value obtained through a call to `OpenAPIClient.Utils.get_config(:default, :operation_pipeline)}
    * `client`: Module that implements `OpenAPIClient` behaviour. Default value obtained through a call to `OpenAPIClient.Utils.get_config(:default, :client, OpenAPIClient)`

  """
  @spec token(LiqPayAPI.InternetAcquiring.Token.Request.t()) ::
          {:ok, LiqPayAPI.InternetAcquiring.Token.Response.t()}
          | {:error, OpenAPIClient.Error.t()}
  @spec token(LiqPayAPI.InternetAcquiring.Token.Request.t(), [
          {:base_url, String.t() | URI.t()}
          | {:pipeline, OpenAPIClient.pipeline()}
          | {:client, module()}
        ]) ::
          {:ok, LiqPayAPI.InternetAcquiring.Token.Response.t()}
          | {:error, OpenAPIClient.Error.t()}
  def token(body, opts \\ []) do
    body =
      case body do
        %LiqPayAPI.InternetAcquiring.Token.Request{public_key: nil} ->
          %LiqPayAPI.InternetAcquiring.Token.Request{
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
        request_types: [{"application/json", {LiqPayAPI.InternetAcquiring.Token.Request, :t}}],
        response_types: [
          {200, [{"application/json", {LiqPayAPI.InternetAcquiring.Token.Response, :t}}]}
        ],
        function_args: [body: body],
        function_call: {__MODULE__, :token},
        function_opts: opts,
        profile: :default
      },
      pipeline
    )
  end

  @doc """
  Internet acquiring. Payment widget

  ## Arguments

    * `body`

  ## Options

    * `base_url`: Request's base URL. Default value is taken from `@base_url`
    * `pipeline`: Operation pipeline for making a request. Default value obtained through a call to `OpenAPIClient.Utils.get_config(:default, :operation_pipeline)}
    * `client`: Module that implements `OpenAPIClient` behaviour. Default value obtained through a call to `OpenAPIClient.Utils.get_config(:default, :client, OpenAPIClient)`

  """
  @spec widget(LiqPayAPI.InternetAcquiring.Widget.Request.t()) ::
          :ok | {:error, OpenAPIClient.Error.t()}
  @spec widget(LiqPayAPI.InternetAcquiring.Widget.Request.t(), [
          {:base_url, String.t() | URI.t()}
          | {:pipeline, OpenAPIClient.pipeline()}
          | {:client, module()}
        ]) :: :ok | {:error, OpenAPIClient.Error.t()}
  def widget(body, opts \\ []) do
    body =
      case body do
        %LiqPayAPI.InternetAcquiring.Widget.Request{public_key: nil} ->
          %LiqPayAPI.InternetAcquiring.Widget.Request{
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
        request_types: [{"application/json", {LiqPayAPI.InternetAcquiring.Widget.Request, :t}}],
        function_args: [body: body],
        function_call: {__MODULE__, :widget},
        function_opts: opts,
        profile: :default
      },
      pipeline
    )
  end
end
