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
    * `pipeline`: Operation pipeline for making a request. Default value obtained through a call to `OpenAPIClient.Utils.get_config(:default, :operation_pipeline)}
    * `client`: Module that implements `OpenAPIClient` behaviour. Default value obtained through a call to `OpenAPIClient.Utils.get_config(:default, :client, OpenAPIClient)`

  """
  @spec available_mcc(LiqPayAPI.Partnership.ShopCreate.AvailableMCC.Request.t()) ::
          {:ok, LiqPayAPI.Partnership.ShopCreate.AvailableMCC.Response.t()}
          | {:error, OpenAPIClient.Error.t()}
  @spec available_mcc(LiqPayAPI.Partnership.ShopCreate.AvailableMCC.Request.t(), [
          {:private_key, String.t()}
          | {:base_url, String.t() | URI.t()}
          | {:pipeline, OpenAPIClient.pipeline()}
          | {:client, module()}
        ]) ::
          {:ok, LiqPayAPI.Partnership.ShopCreate.AvailableMCC.Response.t()}
          | {:error, OpenAPIClient.Error.t()}
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
          {"application/json", {LiqPayAPI.Partnership.ShopCreate.AvailableMCC.Request, :t}}
        ],
        response_types: [
          {200,
           [{"application/json", {LiqPayAPI.Partnership.ShopCreate.AvailableMCC.Response, :t}}]}
        ],
        function_args: [body: body],
        function_call: {__MODULE__, :available_mcc},
        function_opts: opts,
        profile: :default
      },
      pipeline
    )
  end

  @doc """
  Partner's. Company creation. Company creation create

  ## Arguments

    * `body`

  ## Options

    * `private_key`: Private key of the created company (not available to anyone except your developer). Default value obtained through a call to `Application.get_env(:liqpay_api_ex, :private_key)`
    * `base_url`: Request's base URL. Default value is taken from `@base_url`
    * `pipeline`: Operation pipeline for making a request. Default value obtained through a call to `OpenAPIClient.Utils.get_config(:default, :operation_pipeline)}
    * `client`: Module that implements `OpenAPIClient` behaviour. Default value obtained through a call to `OpenAPIClient.Utils.get_config(:default, :client, OpenAPIClient)`

  """
  @spec create(LiqPayAPI.Partnership.ShopCreate.Create.Request.t()) ::
          {:ok, LiqPayAPI.Partnership.ShopCreate.Create.Response.t()}
          | {:error, OpenAPIClient.Error.t()}
  @spec create(LiqPayAPI.Partnership.ShopCreate.Create.Request.t(), [
          {:private_key, String.t()}
          | {:base_url, String.t() | URI.t()}
          | {:pipeline, OpenAPIClient.pipeline()}
          | {:client, module()}
        ]) ::
          {:ok, LiqPayAPI.Partnership.ShopCreate.Create.Response.t()}
          | {:error, OpenAPIClient.Error.t()}
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
          {"application/json", {LiqPayAPI.Partnership.ShopCreate.Create.Request, :t}}
        ],
        response_types: [
          {200, [{"application/json", {LiqPayAPI.Partnership.ShopCreate.Create.Response, :t}}]}
        ],
        function_args: [body: body],
        function_call: {__MODULE__, :create},
        function_opts: opts,
        profile: :default
      },
      pipeline
    )
  end

  @doc """
  Partner's. Company creation. Documents

  ## Arguments

    * `body`

  ## Options

    * `private_key`: Private key of the created company (not available to anyone except your developer). Default value obtained through a call to `Application.get_env(:liqpay_api_ex, :private_key)`
    * `base_url`: Request's base URL. Default value is taken from `@base_url`
    * `pipeline`: Operation pipeline for making a request. Default value obtained through a call to `OpenAPIClient.Utils.get_config(:default, :operation_pipeline)}
    * `client`: Module that implements `OpenAPIClient` behaviour. Default value obtained through a call to `OpenAPIClient.Utils.get_config(:default, :client, OpenAPIClient)`

  """
  @spec documents(LiqPayAPI.Partnership.ShopCreate.Documents.Request.t()) ::
          {:ok, LiqPayAPI.Partnership.ShopCreate.Documents.Response.t()}
          | {:error, OpenAPIClient.Error.t()}
  @spec documents(LiqPayAPI.Partnership.ShopCreate.Documents.Request.t(), [
          {:private_key, String.t()}
          | {:base_url, String.t() | URI.t()}
          | {:pipeline, OpenAPIClient.pipeline()}
          | {:client, module()}
        ]) ::
          {:ok, LiqPayAPI.Partnership.ShopCreate.Documents.Response.t()}
          | {:error, OpenAPIClient.Error.t()}
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
          {"application/json", {LiqPayAPI.Partnership.ShopCreate.Documents.Request, :t}}
        ],
        response_types: [
          {200, [{"application/json", {LiqPayAPI.Partnership.ShopCreate.Documents.Response, :t}}]}
        ],
        function_args: [body: body],
        function_call: {__MODULE__, :documents},
        function_opts: opts,
        profile: :default
      },
      pipeline
    )
  end

  @doc """
  Partner's. Company creation. Company creation register

  ## Arguments

    * `body`

  ## Options

    * `private_key`: Private key of the created company (not available to anyone except your developer). Default value obtained through a call to `Application.get_env(:liqpay_api_ex, :private_key)`
    * `base_url`: Request's base URL. Default value is taken from `@base_url`
    * `pipeline`: Operation pipeline for making a request. Default value obtained through a call to `OpenAPIClient.Utils.get_config(:default, :operation_pipeline)}
    * `client`: Module that implements `OpenAPIClient` behaviour. Default value obtained through a call to `OpenAPIClient.Utils.get_config(:default, :client, OpenAPIClient)`

  """
  @spec register(LiqPayAPI.Partnership.ShopCreate.Register.Request.t()) ::
          {:ok, LiqPayAPI.Partnership.ShopCreate.Register.Response.t()}
          | {:error, OpenAPIClient.Error.t()}
  @spec register(LiqPayAPI.Partnership.ShopCreate.Register.Request.t(), [
          {:private_key, String.t()}
          | {:base_url, String.t() | URI.t()}
          | {:pipeline, OpenAPIClient.pipeline()}
          | {:client, module()}
        ]) ::
          {:ok, LiqPayAPI.Partnership.ShopCreate.Register.Response.t()}
          | {:error, OpenAPIClient.Error.t()}
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
          {"application/json", {LiqPayAPI.Partnership.ShopCreate.Register.Request, :t}}
        ],
        response_types: [
          {200, [{"application/json", {LiqPayAPI.Partnership.ShopCreate.Register.Response, :t}}]}
        ],
        function_args: [body: body],
        function_call: {__MODULE__, :register},
        function_opts: opts,
        profile: :default
      },
      pipeline
    )
  end
end
