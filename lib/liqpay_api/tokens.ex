defmodule LiqPayAPI.Tokens do
  @moduledoc """
  Provides API endpoints related to tokens
  """

  @base_url "https://www.liqpay.ua"

  @doc """
  Tokens. Status change

  ## Arguments

    * `body`

  ## Options

    * `private_key`: Private key of the created company (not available to anyone except your developer). Default value obtained through a call to `Application.get_env(:liqpay_api_ex, :private_key)`
    * `base_url`: Request's base URL. Default value is taken from `@base_url`
    * `client_pipeline`: Client pipeline for making a request. Default value obtained through a call to `OpenAPIClient.Utils.get_config(__operation__, :client_pipeline)}

  """
  @spec change_status(LiqPayAPI.Tokens.ChangeStatus.Request.t(), [
          {:private_key, String.t()}
          | {:base_url, String.t() | URI.t()}
          | {:client_pipeline, OpenAPIClient.Client.pipeline()}
        ]) ::
          {:ok, LiqPayAPI.Tokens.ChangeStatus.Response.t()}
          | {:error, OpenAPIClient.Client.Error.t()}
  def change_status(body, opts \\ []) do
    body =
      case body do
        %LiqPayAPI.Tokens.ChangeStatus.Request{public_key: nil} ->
          %LiqPayAPI.Tokens.ChangeStatus.Request{
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
      request_types: [{"application/json", {LiqPayAPI.Tokens.ChangeStatus.Request, :t}}],
      response_types: [
        {200, [{"application/json", {LiqPayAPI.Tokens.ChangeStatus.Response, :t}}]}
      ]
    }
    |> OpenAPIClient.Client.Operation.put_private(
      __args__: initial_args,
      __call__: {__MODULE__, :change_status},
      __opts__: opts,
      __params__: [private_key: private_key],
      __profile__: :default
    )
    |> client.perform(client_pipeline)
  end

  @doc """
  Tokens. Token obtainment

  ## Arguments

    * `body`

  ## Options

    * `private_key`: Private key of the created company (not available to anyone except your developer). Default value obtained through a call to `Application.get_env(:liqpay_api_ex, :private_key)`
    * `base_url`: Request's base URL. Default value is taken from `@base_url`
    * `client_pipeline`: Client pipeline for making a request. Default value obtained through a call to `OpenAPIClient.Utils.get_config(__operation__, :client_pipeline)}

  """
  @spec obtain(LiqPayAPI.Tokens.Obtain.Request.t(), [
          {:private_key, String.t()}
          | {:base_url, String.t() | URI.t()}
          | {:client_pipeline, OpenAPIClient.Client.pipeline()}
        ]) ::
          {:ok, LiqPayAPI.Tokens.Obtain.Response.t()} | {:error, OpenAPIClient.Client.Error.t()}
  def obtain(body, opts \\ []) do
    body =
      case body do
        %LiqPayAPI.Tokens.Obtain.Request{public_key: nil} ->
          %LiqPayAPI.Tokens.Obtain.Request{
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
      request_types: [{"application/json", {LiqPayAPI.Tokens.Obtain.Request, :t}}],
      response_types: [{200, [{"application/json", {LiqPayAPI.Tokens.Obtain.Response, :t}}]}]
    }
    |> OpenAPIClient.Client.Operation.put_private(
      __args__: initial_args,
      __call__: {__MODULE__, :obtain},
      __opts__: opts,
      __params__: [private_key: private_key],
      __profile__: :default
    )
    |> client.perform(client_pipeline)
  end
end
