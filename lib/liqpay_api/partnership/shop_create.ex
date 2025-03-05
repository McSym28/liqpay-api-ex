defmodule LiqPayAPI.Partnership.ShopCreate do
  @moduledoc """
  Provides API endpoints related to partnership/shop create
  """

  @base_url "https://www.liqpay.ua"

  @doc """
  Partner's. Company creation. Available МСС

  ## Arguments

    * `body`

  ## Options

    * `private_key`: Private key of the created company (not available to anyone except your developer). Default value obtained through a call to `Application.get_env(:liqpay_api_ex, :private_key)`
    * `base_url`: Request's base URL. Default value is taken from `@base_url`
    * `client_pipeline`: Client pipeline for making a request. Default value obtained through a call to `OpenAPIClient.Utils.get_config(__operation__, :client_pipeline)}

  """
  @spec available_mcc(LiqPayAPI.Partnership.ShopCreate.AvailableMCC.Request.t(), [
          {:private_key, String.t()}
          | {:base_url, String.t() | URI.t()}
          | {:client_pipeline, OpenAPIClient.Client.pipeline()}
        ]) ::
          {:ok, LiqPayAPI.Partnership.ShopCreate.AvailableMCC.Response.t()}
          | {:error, OpenAPIClient.Client.Error.t()}
  def available_mcc(body, opts \\ []) do
    body =
      case body do
        %LiqPayAPI.Partnership.ShopCreate.AvailableMCC.Request{public_key: nil} ->
          %LiqPayAPI.Partnership.ShopCreate.AvailableMCC.Request{
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
        {"application/json", {LiqPayAPI.Partnership.ShopCreate.AvailableMCC.Request, :t}}
      ],
      response_types: [
        {200,
         [{"application/json", {LiqPayAPI.Partnership.ShopCreate.AvailableMCC.Response, :t}}]}
      ]
    }
    |> OpenAPIClient.Client.Operation.put_private(
      __args__: initial_args,
      __call__: {__MODULE__, :available_mcc},
      __opts__: opts,
      __params__: [private_key: private_key],
      __profile__: :default
    )
    |> client.perform(client_pipeline)
  end

  @doc """
  Partner's. Company creation. Company creation create

  ## Arguments

    * `body`

  ## Options

    * `private_key`: Private key of the created company (not available to anyone except your developer). Default value obtained through a call to `Application.get_env(:liqpay_api_ex, :private_key)`
    * `base_url`: Request's base URL. Default value is taken from `@base_url`
    * `client_pipeline`: Client pipeline for making a request. Default value obtained through a call to `OpenAPIClient.Utils.get_config(__operation__, :client_pipeline)}

  """
  @spec create(LiqPayAPI.Partnership.ShopCreate.Create.Request.t(), [
          {:private_key, String.t()}
          | {:base_url, String.t() | URI.t()}
          | {:client_pipeline, OpenAPIClient.Client.pipeline()}
        ]) ::
          {:ok, LiqPayAPI.Partnership.ShopCreate.Create.Response.t()}
          | {:error, OpenAPIClient.Client.Error.t()}
  def create(body, opts \\ []) do
    body =
      case body do
        %LiqPayAPI.Partnership.ShopCreate.Create.Request{public_key: nil} ->
          %LiqPayAPI.Partnership.ShopCreate.Create.Request{
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
      request_types: [{"application/json", {LiqPayAPI.Partnership.ShopCreate.Create.Request, :t}}],
      response_types: [
        {200, [{"application/json", {LiqPayAPI.Partnership.ShopCreate.Create.Response, :t}}]}
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
  Partner's. Company creation. Documents

  ## Arguments

    * `body`

  ## Options

    * `private_key`: Private key of the created company (not available to anyone except your developer). Default value obtained through a call to `Application.get_env(:liqpay_api_ex, :private_key)`
    * `base_url`: Request's base URL. Default value is taken from `@base_url`
    * `client_pipeline`: Client pipeline for making a request. Default value obtained through a call to `OpenAPIClient.Utils.get_config(__operation__, :client_pipeline)}

  """
  @spec documents(LiqPayAPI.Partnership.ShopCreate.Documents.Request.t(), [
          {:private_key, String.t()}
          | {:base_url, String.t() | URI.t()}
          | {:client_pipeline, OpenAPIClient.Client.pipeline()}
        ]) ::
          {:ok, LiqPayAPI.Partnership.ShopCreate.Documents.Response.t()}
          | {:error, OpenAPIClient.Client.Error.t()}
  def documents(body, opts \\ []) do
    body =
      case body do
        %LiqPayAPI.Partnership.ShopCreate.Documents.Request{public_key: nil} ->
          %LiqPayAPI.Partnership.ShopCreate.Documents.Request{
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
        {"application/json", {LiqPayAPI.Partnership.ShopCreate.Documents.Request, :t}}
      ],
      response_types: [
        {200, [{"application/json", {LiqPayAPI.Partnership.ShopCreate.Documents.Response, :t}}]}
      ]
    }
    |> OpenAPIClient.Client.Operation.put_private(
      __args__: initial_args,
      __call__: {__MODULE__, :documents},
      __opts__: opts,
      __params__: [private_key: private_key],
      __profile__: :default
    )
    |> client.perform(client_pipeline)
  end

  @doc """
  Partner's. Company creation. Company creation register

  ## Arguments

    * `body`

  ## Options

    * `private_key`: Private key of the created company (not available to anyone except your developer). Default value obtained through a call to `Application.get_env(:liqpay_api_ex, :private_key)`
    * `base_url`: Request's base URL. Default value is taken from `@base_url`
    * `client_pipeline`: Client pipeline for making a request. Default value obtained through a call to `OpenAPIClient.Utils.get_config(__operation__, :client_pipeline)}

  """
  @spec register(LiqPayAPI.Partnership.ShopCreate.Register.Request.t(), [
          {:private_key, String.t()}
          | {:base_url, String.t() | URI.t()}
          | {:client_pipeline, OpenAPIClient.Client.pipeline()}
        ]) ::
          {:ok, LiqPayAPI.Partnership.ShopCreate.Register.Response.t()}
          | {:error, OpenAPIClient.Client.Error.t()}
  def register(body, opts \\ []) do
    body =
      case body do
        %LiqPayAPI.Partnership.ShopCreate.Register.Request{public_key: nil} ->
          %LiqPayAPI.Partnership.ShopCreate.Register.Request{
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
        {"application/json", {LiqPayAPI.Partnership.ShopCreate.Register.Request, :t}}
      ],
      response_types: [
        {200, [{"application/json", {LiqPayAPI.Partnership.ShopCreate.Register.Response, :t}}]}
      ]
    }
    |> OpenAPIClient.Client.Operation.put_private(
      __args__: initial_args,
      __call__: {__MODULE__, :register},
      __opts__: opts,
      __params__: [private_key: private_key],
      __profile__: :default
    )
    |> client.perform(client_pipeline)
  end
end
