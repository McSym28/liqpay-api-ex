defmodule LiqPayAPI.InternetAcquiring.GPay do
  @moduledoc """
  Provides API endpoints related to internet acquiring/g pay
  """

  @base_url "https://www.liqpay.ua"

  @doc """
  Internet acquiring. Google Pay. Decrypted token

  ## Arguments

    * `body`

  ## Options

    * `private_key`: Private key of the created company (not available to anyone except your developer). Default value obtained through a call to `Application.get_env(:liqpay_api_ex, :private_key)`
    * `base_url`: Request's base URL. Default value is taken from `@base_url`
    * `pipeline`: Operation pipeline for making a request. Default value obtained through a call to `OpenAPIClient.Utils.get_config(:default, :operation_pipeline)}
    * `client`: Module that implements `OpenAPIClient` behaviour. Default value obtained through a call to `OpenAPIClient.Utils.get_config(:default, :client, OpenAPIClient)`

  """
  @spec decrypted_token(LiqPayAPI.InternetAcquiring.GPay.DecryptedToken.Request.t()) ::
          :ok | {:error, OpenAPIClient.Error.t()}
  @spec decrypted_token(LiqPayAPI.InternetAcquiring.GPay.DecryptedToken.Request.t(), [
          {:private_key, String.t()}
          | {:base_url, String.t() | URI.t()}
          | {:pipeline, OpenAPIClient.pipeline()}
          | {:client, module()}
        ]) :: :ok | {:error, OpenAPIClient.Error.t()}
  def decrypted_token(body, opts \\ []) do
    body =
      case body do
        %LiqPayAPI.InternetAcquiring.GPay.DecryptedToken.Request{public_key: nil} ->
          %LiqPayAPI.InternetAcquiring.GPay.DecryptedToken.Request{
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
          {"application/json", {LiqPayAPI.InternetAcquiring.GPay.DecryptedToken.Request, :t}}
        ],
        function_args: [body: body],
        function_call: {__MODULE__, :decrypted_token},
        function_opts: opts,
        profile: :default
      },
      pipeline
    )
  end

  @doc """
  Internet acquiring. Google Pay. Encrypted token

  ## Arguments

    * `body`

  ## Options

    * `private_key`: Private key of the created company (not available to anyone except your developer). Default value obtained through a call to `Application.get_env(:liqpay_api_ex, :private_key)`
    * `base_url`: Request's base URL. Default value is taken from `@base_url`
    * `pipeline`: Operation pipeline for making a request. Default value obtained through a call to `OpenAPIClient.Utils.get_config(:default, :operation_pipeline)}
    * `client`: Module that implements `OpenAPIClient` behaviour. Default value obtained through a call to `OpenAPIClient.Utils.get_config(:default, :client, OpenAPIClient)`

  """
  @spec encrypted_token(LiqPayAPI.InternetAcquiring.GPay.EncryptedToken.Request.t()) ::
          :ok | {:error, OpenAPIClient.Error.t()}
  @spec encrypted_token(LiqPayAPI.InternetAcquiring.GPay.EncryptedToken.Request.t(), [
          {:private_key, String.t()}
          | {:base_url, String.t() | URI.t()}
          | {:pipeline, OpenAPIClient.pipeline()}
          | {:client, module()}
        ]) :: :ok | {:error, OpenAPIClient.Error.t()}
  def encrypted_token(body, opts \\ []) do
    body =
      case body do
        %LiqPayAPI.InternetAcquiring.GPay.EncryptedToken.Request{public_key: nil} ->
          %LiqPayAPI.InternetAcquiring.GPay.EncryptedToken.Request{
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
          {"application/json", {LiqPayAPI.InternetAcquiring.GPay.EncryptedToken.Request, :t}}
        ],
        function_args: [body: body],
        function_call: {__MODULE__, :encrypted_token},
        function_opts: opts,
        profile: :default
      },
      pipeline
    )
  end
end
