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
    * `client_pipeline`: Client pipeline for making a request. Default value obtained through a call to `OpenAPIClient.Utils.get_config(__operation__, :client_pipeline)}

  """
  @spec decrypted_token(LiqPayAPI.InternetAcquiring.GPay.DecryptedToken.Request.t(), [
          {:private_key, String.t()}
          | {:base_url, String.t() | URI.t()}
          | {:client_pipeline, OpenAPIClient.Client.pipeline()}
        ]) :: :ok | {:error, OpenAPIClient.Client.Error.t()}
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
        {"application/json", {LiqPayAPI.InternetAcquiring.GPay.DecryptedToken.Request, :t}}
      ]
    }
    |> OpenAPIClient.Client.Operation.put_private(
      __args__: initial_args,
      __call__: {__MODULE__, :decrypted_token},
      __opts__: opts,
      __params__: [private_key: private_key],
      __profile__: :default
    )
    |> client.perform(client_pipeline)
  end

  @doc """
  Internet acquiring. Google Pay. Encrypted token

  ## Arguments

    * `body`

  ## Options

    * `private_key`: Private key of the created company (not available to anyone except your developer). Default value obtained through a call to `Application.get_env(:liqpay_api_ex, :private_key)`
    * `base_url`: Request's base URL. Default value is taken from `@base_url`
    * `client_pipeline`: Client pipeline for making a request. Default value obtained through a call to `OpenAPIClient.Utils.get_config(__operation__, :client_pipeline)}

  """
  @spec encrypted_token(LiqPayAPI.InternetAcquiring.GPay.EncryptedToken.Request.t(), [
          {:private_key, String.t()}
          | {:base_url, String.t() | URI.t()}
          | {:client_pipeline, OpenAPIClient.Client.pipeline()}
        ]) :: :ok | {:error, OpenAPIClient.Client.Error.t()}
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
        {"application/json", {LiqPayAPI.InternetAcquiring.GPay.EncryptedToken.Request, :t}}
      ]
    }
    |> OpenAPIClient.Client.Operation.put_private(
      __args__: initial_args,
      __call__: {__MODULE__, :encrypted_token},
      __opts__: opts,
      __params__: [private_key: private_key],
      __profile__: :default
    )
    |> client.perform(client_pipeline)
  end
end
