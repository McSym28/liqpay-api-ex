defmodule LiqPayAPI.InternetAcquiring.APay do
  @moduledoc """
  Provides API endpoints related to internet acquiring/a pay
  """

  @base_url "https://www.liqpay.ua"

  @doc """
  Internet acquiring. Apple Pay. Decrypted token

  ## Arguments

    * `body`

  ## Options

    * `base_url`: Request's base URL. Default value is taken from `@base_url`
    * `pipeline`: Operation pipeline for making a request. Default value obtained through a call to `OpenAPIClient.Utils.get_config(:default, :operation_pipeline)}
    * `client`: Module that implements `OpenAPIClient` behaviour. Default value obtained through a call to `OpenAPIClient.Utils.get_config(:default, :client, OpenAPIClient)`

  """
  @spec decrypted_token(LiqPayAPI.InternetAcquiring.APay.DecryptedToken.Request.t()) ::
          :ok | {:error, OpenAPIClient.Error.t()}
  @spec decrypted_token(LiqPayAPI.InternetAcquiring.APay.DecryptedToken.Request.t(), [
          {:base_url, String.t() | URI.t()}
          | {:pipeline, OpenAPIClient.pipeline()}
          | {:client, module()}
        ]) :: :ok | {:error, OpenAPIClient.Error.t()}
  def decrypted_token(body, opts \\ []) do
    body =
      case body do
        %LiqPayAPI.InternetAcquiring.APay.DecryptedToken.Request{public_key: nil} ->
          %LiqPayAPI.InternetAcquiring.APay.DecryptedToken.Request{
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
        request_path: "/api/request?path=internet_acquiring.apay.decrypted_token",
        method: :post,
        request_types: [
          {"application/json", {LiqPayAPI.InternetAcquiring.APay.DecryptedToken.Request, :t}}
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
  Internet acquiring. Apple Pay. Encrypted token

  ## Arguments

    * `body`

  ## Options

    * `base_url`: Request's base URL. Default value is taken from `@base_url`
    * `pipeline`: Operation pipeline for making a request. Default value obtained through a call to `OpenAPIClient.Utils.get_config(:default, :operation_pipeline)}
    * `client`: Module that implements `OpenAPIClient` behaviour. Default value obtained through a call to `OpenAPIClient.Utils.get_config(:default, :client, OpenAPIClient)`

  """
  @spec encrypted_token(LiqPayAPI.InternetAcquiring.APay.EncryptedToken.Request.t()) ::
          :ok | {:error, OpenAPIClient.Error.t()}
  @spec encrypted_token(LiqPayAPI.InternetAcquiring.APay.EncryptedToken.Request.t(), [
          {:base_url, String.t() | URI.t()}
          | {:pipeline, OpenAPIClient.pipeline()}
          | {:client, module()}
        ]) :: :ok | {:error, OpenAPIClient.Error.t()}
  def encrypted_token(body, opts \\ []) do
    body =
      case body do
        %LiqPayAPI.InternetAcquiring.APay.EncryptedToken.Request{public_key: nil} ->
          %LiqPayAPI.InternetAcquiring.APay.EncryptedToken.Request{
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
        request_path: "/api/request?path=internet_acquiring.apay.encrypted_token",
        method: :post,
        request_types: [
          {"application/json", {LiqPayAPI.InternetAcquiring.APay.EncryptedToken.Request, :t}}
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
