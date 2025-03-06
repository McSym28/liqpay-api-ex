defmodule LiqPayAPI.Partnership do
  @moduledoc """
  Provides API endpoints related to partnership
  """

  @base_url "https://www.liqpay.ua"

  @doc """
  Partner's. Information about company

  ## Arguments

    * `body`

  ## Options

    * `base_url`: Request's base URL. Default value is taken from `@base_url`
    * `pipeline`: Operation pipeline for making a request. Default value obtained through a call to `OpenAPIClient.Utils.get_config(:default, :operation_pipeline)}
    * `client`: Module that implements `OpenAPIClient` behaviour. Default value obtained through a call to `OpenAPIClient.Utils.get_config(:default, :client, OpenAPIClient)`

  """
  @spec info_merchant(LiqPayAPI.Partnership.InfoMerchant.Request.t()) ::
          {:ok, LiqPayAPI.Partnership.InfoMerchant.Response.t()}
          | {:error, OpenAPIClient.Error.t()}
  @spec info_merchant(LiqPayAPI.Partnership.InfoMerchant.Request.t(), [
          {:base_url, String.t() | URI.t()}
          | {:pipeline, OpenAPIClient.pipeline()}
          | {:client, module()}
        ]) ::
          {:ok, LiqPayAPI.Partnership.InfoMerchant.Response.t()}
          | {:error, OpenAPIClient.Error.t()}
  def info_merchant(body, opts \\ []) do
    body =
      case body do
        %LiqPayAPI.Partnership.InfoMerchant.Request{public_key: nil} ->
          %LiqPayAPI.Partnership.InfoMerchant.Request{
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
        request_types: [{"application/json", {LiqPayAPI.Partnership.InfoMerchant.Request, :t}}],
        response_types: [
          {200, [{"application/json", {LiqPayAPI.Partnership.InfoMerchant.Response, :t}}]}
        ],
        function_args: [body: body],
        function_call: {__MODULE__, :info_merchant},
        function_opts: opts,
        profile: :default
      },
      pipeline
    )
  end

  @doc """
  Partner's. Information about partner

  ## Arguments

    * `body`

  ## Options

    * `base_url`: Request's base URL. Default value is taken from `@base_url`
    * `pipeline`: Operation pipeline for making a request. Default value obtained through a call to `OpenAPIClient.Utils.get_config(:default, :operation_pipeline)}
    * `client`: Module that implements `OpenAPIClient` behaviour. Default value obtained through a call to `OpenAPIClient.Utils.get_config(:default, :client, OpenAPIClient)`

  """
  @spec info_user(LiqPayAPI.Partnership.InfoUser.Request.t()) ::
          {:ok, LiqPayAPI.Partnership.InfoUser.Response.t()} | {:error, OpenAPIClient.Error.t()}
  @spec info_user(LiqPayAPI.Partnership.InfoUser.Request.t(), [
          {:base_url, String.t() | URI.t()}
          | {:pipeline, OpenAPIClient.pipeline()}
          | {:client, module()}
        ]) ::
          {:ok, LiqPayAPI.Partnership.InfoUser.Response.t()} | {:error, OpenAPIClient.Error.t()}
  def info_user(body, opts \\ []) do
    body =
      case body do
        %LiqPayAPI.Partnership.InfoUser.Request{public_key: nil} ->
          %LiqPayAPI.Partnership.InfoUser.Request{
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
        request_types: [{"application/json", {LiqPayAPI.Partnership.InfoUser.Request, :t}}],
        response_types: [
          {200, [{"application/json", {LiqPayAPI.Partnership.InfoUser.Response, :t}}]}
        ],
        function_args: [body: body],
        function_call: {__MODULE__, :info_user},
        function_opts: opts,
        profile: :default
      },
      pipeline
    )
  end

  @doc """
  Partner's. Company editing

  ## Arguments

    * `body`

  ## Options

    * `base_url`: Request's base URL. Default value is taken from `@base_url`
    * `pipeline`: Operation pipeline for making a request. Default value obtained through a call to `OpenAPIClient.Utils.get_config(:default, :operation_pipeline)}
    * `client`: Module that implements `OpenAPIClient` behaviour. Default value obtained through a call to `OpenAPIClient.Utils.get_config(:default, :client, OpenAPIClient)`

  """
  @spec shop_edit(LiqPayAPI.Partnership.ShopEdit.Request.t()) ::
          {:ok, LiqPayAPI.Partnership.ShopEdit.Response.t()} | {:error, OpenAPIClient.Error.t()}
  @spec shop_edit(LiqPayAPI.Partnership.ShopEdit.Request.t(), [
          {:base_url, String.t() | URI.t()}
          | {:pipeline, OpenAPIClient.pipeline()}
          | {:client, module()}
        ]) ::
          {:ok, LiqPayAPI.Partnership.ShopEdit.Response.t()} | {:error, OpenAPIClient.Error.t()}
  def shop_edit(body, opts \\ []) do
    body =
      case body do
        %LiqPayAPI.Partnership.ShopEdit.Request{public_key: nil} ->
          %LiqPayAPI.Partnership.ShopEdit.Request{
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
        request_types: [{"application/json", {LiqPayAPI.Partnership.ShopEdit.Request, :t}}],
        response_types: [
          {200, [{"application/json", {LiqPayAPI.Partnership.ShopEdit.Response, :t}}]}
        ],
        function_args: [body: body],
        function_call: {__MODULE__, :shop_edit},
        function_opts: opts,
        profile: :default
      },
      pipeline
    )
  end
end
