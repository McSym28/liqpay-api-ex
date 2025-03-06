defmodule LiqPayAPI.Confirmation do
  @moduledoc """
  Provides API endpoints related to confirmation
  """

  @base_url "https://www.liqpay.ua"

  @doc """
  Verification. Cardverification

  ## Arguments

    * `body`

  ## Options

    * `base_url`: Request's base URL. Default value is taken from `@base_url`
    * `pipeline`: Operation pipeline for making a request. Default value obtained through a call to `OpenAPIClient.Utils.get_config(:default, :operation_pipeline)}
    * `client`: Module that implements `OpenAPIClient` behaviour. Default value obtained through a call to `OpenAPIClient.Utils.get_config(:default, :client, OpenAPIClient)`

  """
  @spec card_verification(LiqPayAPI.Confirmation.CardVerification.Request.t()) ::
          {:ok, LiqPayAPI.Confirmation.CardVerification.Response.t()}
          | {:error, OpenAPIClient.Error.t()}
  @spec card_verification(LiqPayAPI.Confirmation.CardVerification.Request.t(), [
          {:base_url, String.t() | URI.t()}
          | {:pipeline, OpenAPIClient.pipeline()}
          | {:client, module()}
        ]) ::
          {:ok, LiqPayAPI.Confirmation.CardVerification.Response.t()}
          | {:error, OpenAPIClient.Error.t()}
  def card_verification(body, opts \\ []) do
    body =
      case body do
        %LiqPayAPI.Confirmation.CardVerification.Request{public_key: nil} ->
          %LiqPayAPI.Confirmation.CardVerification.Request{
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
          {"application/json", {LiqPayAPI.Confirmation.CardVerification.Request, :t}}
        ],
        response_types: [
          {200, [{"application/json", {LiqPayAPI.Confirmation.CardVerification.Response, :t}}]}
        ],
        function_args: [body: body],
        function_call: {__MODULE__, :card_verification},
        function_opts: opts,
        profile: :default
      },
      pipeline
    )
  end

  @doc """
  Verification. CVV confirmation

  ## Arguments

    * `body`

  ## Options

    * `base_url`: Request's base URL. Default value is taken from `@base_url`
    * `pipeline`: Operation pipeline for making a request. Default value obtained through a call to `OpenAPIClient.Utils.get_config(:default, :operation_pipeline)}
    * `client`: Module that implements `OpenAPIClient` behaviour. Default value obtained through a call to `OpenAPIClient.Utils.get_config(:default, :client, OpenAPIClient)`

  """
  @spec cvv(LiqPayAPI.Confirmation.CVV.Request.t()) ::
          {:ok, LiqPayAPI.Confirmation.CVV.Response.t()} | {:error, OpenAPIClient.Error.t()}
  @spec cvv(LiqPayAPI.Confirmation.CVV.Request.t(), [
          {:base_url, String.t() | URI.t()}
          | {:pipeline, OpenAPIClient.pipeline()}
          | {:client, module()}
        ]) :: {:ok, LiqPayAPI.Confirmation.CVV.Response.t()} | {:error, OpenAPIClient.Error.t()}
  def cvv(body, opts \\ []) do
    body =
      case body do
        %LiqPayAPI.Confirmation.CVV.Request{public_key: nil} ->
          %LiqPayAPI.Confirmation.CVV.Request{
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
        request_types: [{"application/json", {LiqPayAPI.Confirmation.CVV.Request, :t}}],
        response_types: [{200, [{"application/json", {LiqPayAPI.Confirmation.CVV.Response, :t}}]}],
        function_args: [body: body],
        function_call: {__MODULE__, :cvv},
        function_opts: opts,
        profile: :default
      },
      pipeline
    )
  end

  @doc """
  Verification. MPI

  ## Arguments

    * `body`

  ## Options

    * `base_url`: Request's base URL. Default value is taken from `@base_url`
    * `pipeline`: Operation pipeline for making a request. Default value obtained through a call to `OpenAPIClient.Utils.get_config(:default, :operation_pipeline)}
    * `client`: Module that implements `OpenAPIClient` behaviour. Default value obtained through a call to `OpenAPIClient.Utils.get_config(:default, :client, OpenAPIClient)`

  """
  @spec mpi(LiqPayAPI.Confirmation.MPI.Request.t()) ::
          {:ok, LiqPayAPI.Confirmation.MPI.Response.t()} | {:error, OpenAPIClient.Error.t()}
  @spec mpi(LiqPayAPI.Confirmation.MPI.Request.t(), [
          {:base_url, String.t() | URI.t()}
          | {:pipeline, OpenAPIClient.pipeline()}
          | {:client, module()}
        ]) :: {:ok, LiqPayAPI.Confirmation.MPI.Response.t()} | {:error, OpenAPIClient.Error.t()}
  def mpi(body, opts \\ []) do
    body =
      case body do
        %LiqPayAPI.Confirmation.MPI.Request{public_key: nil} ->
          %LiqPayAPI.Confirmation.MPI.Request{
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
        request_types: [{"application/json", {LiqPayAPI.Confirmation.MPI.Request, :t}}],
        response_types: [{200, [{"application/json", {LiqPayAPI.Confirmation.MPI.Response, :t}}]}],
        function_args: [body: body],
        function_call: {__MODULE__, :mpi},
        function_opts: opts,
        profile: :default
      },
      pipeline
    )
  end

  @doc """
  Verification. OTP confirmation

  ## Arguments

    * `body`

  ## Options

    * `base_url`: Request's base URL. Default value is taken from `@base_url`
    * `pipeline`: Operation pipeline for making a request. Default value obtained through a call to `OpenAPIClient.Utils.get_config(:default, :operation_pipeline)}
    * `client`: Module that implements `OpenAPIClient` behaviour. Default value obtained through a call to `OpenAPIClient.Utils.get_config(:default, :client, OpenAPIClient)`

  """
  @spec otp(LiqPayAPI.Confirmation.OTP.Request.t()) ::
          {:ok, LiqPayAPI.Confirmation.OTP.Response.t()} | {:error, OpenAPIClient.Error.t()}
  @spec otp(LiqPayAPI.Confirmation.OTP.Request.t(), [
          {:base_url, String.t() | URI.t()}
          | {:pipeline, OpenAPIClient.pipeline()}
          | {:client, module()}
        ]) :: {:ok, LiqPayAPI.Confirmation.OTP.Response.t()} | {:error, OpenAPIClient.Error.t()}
  def otp(body, opts \\ []) do
    body =
      case body do
        %LiqPayAPI.Confirmation.OTP.Request{public_key: nil} ->
          %LiqPayAPI.Confirmation.OTP.Request{
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
        request_types: [{"application/json", {LiqPayAPI.Confirmation.OTP.Request, :t}}],
        response_types: [{200, [{"application/json", {LiqPayAPI.Confirmation.OTP.Response, :t}}]}],
        function_args: [body: body],
        function_call: {__MODULE__, :otp},
        function_opts: opts,
        profile: :default
      },
      pipeline
    )
  end

  @doc """
  Verification. Recipient's verification

  ## Arguments

    * `body`

  ## Options

    * `base_url`: Request's base URL. Default value is taken from `@base_url`
    * `pipeline`: Operation pipeline for making a request. Default value obtained through a call to `OpenAPIClient.Utils.get_config(:default, :operation_pipeline)}
    * `client`: Module that implements `OpenAPIClient` behaviour. Default value obtained through a call to `OpenAPIClient.Utils.get_config(:default, :client, OpenAPIClient)`

  """
  @spec receiver_verify(LiqPayAPI.Confirmation.ReceiverVerify.Request.t()) ::
          {:ok, LiqPayAPI.Confirmation.ReceiverVerify.Response.t()}
          | {:error, OpenAPIClient.Error.t()}
  @spec receiver_verify(LiqPayAPI.Confirmation.ReceiverVerify.Request.t(), [
          {:base_url, String.t() | URI.t()}
          | {:pipeline, OpenAPIClient.pipeline()}
          | {:client, module()}
        ]) ::
          {:ok, LiqPayAPI.Confirmation.ReceiverVerify.Response.t()}
          | {:error, OpenAPIClient.Error.t()}
  def receiver_verify(body, opts \\ []) do
    body =
      case body do
        %LiqPayAPI.Confirmation.ReceiverVerify.Request{public_key: nil} ->
          %LiqPayAPI.Confirmation.ReceiverVerify.Request{
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
        request_types: [{"application/json", {LiqPayAPI.Confirmation.ReceiverVerify.Request, :t}}],
        response_types: [
          {200, [{"application/json", {LiqPayAPI.Confirmation.ReceiverVerify.Response, :t}}]}
        ],
        function_args: [body: body],
        function_call: {__MODULE__, :receiver_verify},
        function_opts: opts,
        profile: :default
      },
      pipeline
    )
  end

  @doc """
  Verification. Sender's verification

  ## Arguments

    * `body`

  ## Options

    * `base_url`: Request's base URL. Default value is taken from `@base_url`
    * `pipeline`: Operation pipeline for making a request. Default value obtained through a call to `OpenAPIClient.Utils.get_config(:default, :operation_pipeline)}
    * `client`: Module that implements `OpenAPIClient` behaviour. Default value obtained through a call to `OpenAPIClient.Utils.get_config(:default, :client, OpenAPIClient)`

  """
  @spec sender_verify(LiqPayAPI.Confirmation.SenderVerify.Request.t()) ::
          {:ok, LiqPayAPI.Confirmation.SenderVerify.Response.t()}
          | {:error, OpenAPIClient.Error.t()}
  @spec sender_verify(LiqPayAPI.Confirmation.SenderVerify.Request.t(), [
          {:base_url, String.t() | URI.t()}
          | {:pipeline, OpenAPIClient.pipeline()}
          | {:client, module()}
        ]) ::
          {:ok, LiqPayAPI.Confirmation.SenderVerify.Response.t()}
          | {:error, OpenAPIClient.Error.t()}
  def sender_verify(body, opts \\ []) do
    body =
      case body do
        %LiqPayAPI.Confirmation.SenderVerify.Request{public_key: nil} ->
          %LiqPayAPI.Confirmation.SenderVerify.Request{
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
        request_types: [{"application/json", {LiqPayAPI.Confirmation.SenderVerify.Request, :t}}],
        response_types: [
          {200, [{"application/json", {LiqPayAPI.Confirmation.SenderVerify.Response, :t}}]}
        ],
        function_args: [body: body],
        function_call: {__MODULE__, :sender_verify},
        function_opts: opts,
        profile: :default
      },
      pipeline
    )
  end

  @doc """
  Verification. 3D-Secure

  ## Arguments

    * `body`

  ## Options

    * `base_url`: Request's base URL. Default value is taken from `@base_url`
    * `pipeline`: Operation pipeline for making a request. Default value obtained through a call to `OpenAPIClient.Utils.get_config(:default, :operation_pipeline)}
    * `client`: Module that implements `OpenAPIClient` behaviour. Default value obtained through a call to `OpenAPIClient.Utils.get_config(:default, :client, OpenAPIClient)`

  """
  @spec three_ds(LiqPayAPI.Confirmation.ThreeDS.Request.t()) ::
          {:ok, LiqPayAPI.Confirmation.ThreeDS.Response.t()} | {:error, OpenAPIClient.Error.t()}
  @spec three_ds(LiqPayAPI.Confirmation.ThreeDS.Request.t(), [
          {:base_url, String.t() | URI.t()}
          | {:pipeline, OpenAPIClient.pipeline()}
          | {:client, module()}
        ]) ::
          {:ok, LiqPayAPI.Confirmation.ThreeDS.Response.t()} | {:error, OpenAPIClient.Error.t()}
  def three_ds(body, opts \\ []) do
    body =
      case body do
        %LiqPayAPI.Confirmation.ThreeDS.Request{public_key: nil} ->
          %LiqPayAPI.Confirmation.ThreeDS.Request{
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
        request_types: [{"application/json", {LiqPayAPI.Confirmation.ThreeDS.Request, :t}}],
        response_types: [
          {200, [{"application/json", {LiqPayAPI.Confirmation.ThreeDS.Response, :t}}]}
        ],
        function_args: [body: body],
        function_call: {__MODULE__, :three_ds},
        function_opts: opts,
        profile: :default
      },
      pipeline
    )
  end
end
