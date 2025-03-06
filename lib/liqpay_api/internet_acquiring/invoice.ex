defmodule LiqPayAPI.InternetAcquiring.Invoice do
  @moduledoc """
  Provides API endpoints related to internet acquiring/invoice
  """

  @base_url "https://www.liqpay.ua"

  @doc """
  Internet acquiring. Invoice. Invoice cancelation

  ## Arguments

    * `body`

  ## Options

    * `base_url`: Request's base URL. Default value is taken from `@base_url`
    * `pipeline`: Operation pipeline for making a request. Default value obtained through a call to `OpenAPIClient.Utils.get_config(:default, :operation_pipeline)}
    * `client`: Module that implements `OpenAPIClient` behaviour. Default value obtained through a call to `OpenAPIClient.Utils.get_config(:default, :client, OpenAPIClient)`

  """
  @spec cancel(LiqPayAPI.InternetAcquiring.Invoice.Cancel.Request.t()) ::
          {:ok, LiqPayAPI.InternetAcquiring.Invoice.Cancel.Response.t()}
          | {:error, OpenAPIClient.Error.t()}
  @spec cancel(LiqPayAPI.InternetAcquiring.Invoice.Cancel.Request.t(), [
          {:base_url, String.t() | URI.t()}
          | {:pipeline, OpenAPIClient.pipeline()}
          | {:client, module()}
        ]) ::
          {:ok, LiqPayAPI.InternetAcquiring.Invoice.Cancel.Response.t()}
          | {:error, OpenAPIClient.Error.t()}
  def cancel(body, opts \\ []) do
    body =
      case body do
        %LiqPayAPI.InternetAcquiring.Invoice.Cancel.Request{public_key: nil} ->
          %LiqPayAPI.InternetAcquiring.Invoice.Cancel.Request{
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
          {"application/json", {LiqPayAPI.InternetAcquiring.Invoice.Cancel.Request, :t}}
        ],
        response_types: [
          {200, [{"application/json", {LiqPayAPI.InternetAcquiring.Invoice.Cancel.Response, :t}}]}
        ],
        function_args: [body: body],
        function_call: {__MODULE__, :cancel},
        function_opts: opts,
        profile: :default
      },
      pipeline
    )
  end

  @doc """
  Internet acquiring. Invoice. Issuing the invoice

  ## Arguments

    * `body`

  ## Options

    * `base_url`: Request's base URL. Default value is taken from `@base_url`
    * `pipeline`: Operation pipeline for making a request. Default value obtained through a call to `OpenAPIClient.Utils.get_config(:default, :operation_pipeline)}
    * `client`: Module that implements `OpenAPIClient` behaviour. Default value obtained through a call to `OpenAPIClient.Utils.get_config(:default, :client, OpenAPIClient)`

  """
  @spec issue(LiqPayAPI.InternetAcquiring.Invoice.Issue.Request.t()) ::
          {:ok, LiqPayAPI.InternetAcquiring.Invoice.Issue.Response.t()}
          | {:error, OpenAPIClient.Error.t()}
  @spec issue(LiqPayAPI.InternetAcquiring.Invoice.Issue.Request.t(), [
          {:base_url, String.t() | URI.t()}
          | {:pipeline, OpenAPIClient.pipeline()}
          | {:client, module()}
        ]) ::
          {:ok, LiqPayAPI.InternetAcquiring.Invoice.Issue.Response.t()}
          | {:error, OpenAPIClient.Error.t()}
  def issue(body, opts \\ []) do
    body =
      case body do
        %LiqPayAPI.InternetAcquiring.Invoice.Issue.Request{public_key: nil} ->
          %LiqPayAPI.InternetAcquiring.Invoice.Issue.Request{
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
          {"application/json", {LiqPayAPI.InternetAcquiring.Invoice.Issue.Request, :t}}
        ],
        response_types: [
          {200, [{"application/json", {LiqPayAPI.InternetAcquiring.Invoice.Issue.Response, :t}}]}
        ],
        function_args: [body: body],
        function_call: {__MODULE__, :issue},
        function_opts: opts,
        profile: :default
      },
      pipeline
    )
  end

  @doc """
  Internet acquiring. Invoice. 

  ## Arguments

    * `body`

  ## Options

    * `base_url`: Request's base URL. Default value is taken from `@base_url`
    * `pipeline`: Operation pipeline for making a request. Default value obtained through a call to `OpenAPIClient.Utils.get_config(:default, :operation_pipeline)}
    * `client`: Module that implements `OpenAPIClient` behaviour. Default value obtained through a call to `OpenAPIClient.Utils.get_config(:default, :client, OpenAPIClient)`

  """
  @spec units(LiqPayAPI.InternetAcquiring.Invoice.Units.Request.t()) ::
          {:ok,
           LiqPayAPI.InternetAcquiring.Invoice.Units.Response.MultiLanguage.t()
           | LiqPayAPI.InternetAcquiring.Invoice.Units.Response.SingleLanguage.t()}
          | {:error, OpenAPIClient.Error.t()}
  @spec units(LiqPayAPI.InternetAcquiring.Invoice.Units.Request.t(), [
          {:base_url, String.t() | URI.t()}
          | {:pipeline, OpenAPIClient.pipeline()}
          | {:client, module()}
        ]) ::
          {:ok,
           LiqPayAPI.InternetAcquiring.Invoice.Units.Response.MultiLanguage.t()
           | LiqPayAPI.InternetAcquiring.Invoice.Units.Response.SingleLanguage.t()}
          | {:error, OpenAPIClient.Error.t()}
  def units(body, opts \\ []) do
    pipeline = opts[:pipeline] || OpenAPIClient.Utils.get_config(:default, :operation_pipeline)
    base_url = opts[:base_url] || @base_url
    client = opts[:client] || OpenAPIClient.Utils.get_config(:default, :client, OpenAPIClient)

    client.operation(
      %OpenAPIClient.State{
        request_base_url: base_url,
        request_path: "/api/request",
        method: :post,
        request_types: [
          {"application/json", {LiqPayAPI.InternetAcquiring.Invoice.Units.Request, :t}}
        ],
        response_types: [
          {200,
           [
             {"application/json",
              {:union,
               [
                 {LiqPayAPI.InternetAcquiring.Invoice.Units.Response.MultiLanguage, :t},
                 {LiqPayAPI.InternetAcquiring.Invoice.Units.Response.SingleLanguage, :t}
               ]}}
           ]}
        ],
        function_args: [body: body],
        function_call: {__MODULE__, :units},
        function_opts: opts,
        profile: :default
      },
      pipeline
    )
  end
end
