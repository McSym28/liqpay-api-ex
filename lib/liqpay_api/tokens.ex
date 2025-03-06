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

    * `base_url`: Request's base URL. Default value is taken from `@base_url`
    * `pipeline`: Operation pipeline for making a request. Default value obtained through a call to `OpenAPIClient.Utils.get_config(:default, :operation_pipeline)}
    * `client`: Module that implements `OpenAPIClient` behaviour. Default value obtained through a call to `OpenAPIClient.Utils.get_config(:default, :client, OpenAPIClient)`

  """
  @spec change_status(LiqPayAPI.Tokens.ChangeStatus.Request.t()) ::
          {:ok, LiqPayAPI.Tokens.ChangeStatus.Response.t()} | {:error, OpenAPIClient.Error.t()}
  @spec change_status(LiqPayAPI.Tokens.ChangeStatus.Request.t(), [
          {:base_url, String.t() | URI.t()}
          | {:pipeline, OpenAPIClient.pipeline()}
          | {:client, module()}
        ]) ::
          {:ok, LiqPayAPI.Tokens.ChangeStatus.Response.t()} | {:error, OpenAPIClient.Error.t()}
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

    pipeline = opts[:pipeline] || OpenAPIClient.Utils.get_config(:default, :operation_pipeline)
    base_url = opts[:base_url] || @base_url
    client = opts[:client] || OpenAPIClient.Utils.get_config(:default, :client, OpenAPIClient)

    client.operation(
      %OpenAPIClient.State{
        request_base_url: base_url,
        request_path: "/api/request",
        method: :post,
        request_types: [{"application/json", {LiqPayAPI.Tokens.ChangeStatus.Request, :t}}],
        response_types: [
          {200, [{"application/json", {LiqPayAPI.Tokens.ChangeStatus.Response, :t}}]}
        ],
        function_args: [body: body],
        function_call: {__MODULE__, :change_status},
        function_opts: opts,
        profile: :default
      },
      pipeline
    )
  end

  @doc """
  Tokens. Token obtainment

  ## Arguments

    * `body`

  ## Options

    * `base_url`: Request's base URL. Default value is taken from `@base_url`
    * `pipeline`: Operation pipeline for making a request. Default value obtained through a call to `OpenAPIClient.Utils.get_config(:default, :operation_pipeline)}
    * `client`: Module that implements `OpenAPIClient` behaviour. Default value obtained through a call to `OpenAPIClient.Utils.get_config(:default, :client, OpenAPIClient)`

  """
  @spec obtain(LiqPayAPI.Tokens.Obtain.Request.t()) ::
          {:ok, LiqPayAPI.Tokens.Obtain.Response.t()} | {:error, OpenAPIClient.Error.t()}
  @spec obtain(LiqPayAPI.Tokens.Obtain.Request.t(), [
          {:base_url, String.t() | URI.t()}
          | {:pipeline, OpenAPIClient.pipeline()}
          | {:client, module()}
        ]) :: {:ok, LiqPayAPI.Tokens.Obtain.Response.t()} | {:error, OpenAPIClient.Error.t()}
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

    pipeline = opts[:pipeline] || OpenAPIClient.Utils.get_config(:default, :operation_pipeline)
    base_url = opts[:base_url] || @base_url
    client = opts[:client] || OpenAPIClient.Utils.get_config(:default, :client, OpenAPIClient)

    client.operation(
      %OpenAPIClient.State{
        request_base_url: base_url,
        request_path: "/api/request",
        method: :post,
        request_types: [{"application/json", {LiqPayAPI.Tokens.Obtain.Request, :t}}],
        response_types: [{200, [{"application/json", {LiqPayAPI.Tokens.Obtain.Response, :t}}]}],
        function_args: [body: body],
        function_call: {__MODULE__, :obtain},
        function_opts: opts,
        profile: :default
      },
      pipeline
    )
  end
end
