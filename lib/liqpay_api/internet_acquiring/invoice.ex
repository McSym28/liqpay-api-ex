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

    * `private_key`: Private key of the created company (not available to anyone except your developer). Default value obtained through a call to `Application.get_env(:liqpay_api_ex, :private_key)`
    * `base_url`: Request's base URL. Default value is taken from `@base_url`
    * `client_pipeline`: Client pipeline for making a request. Default value obtained through a call to `OpenAPIClient.Utils.get_config(__operation__, :client_pipeline)}

  """
  @spec cancel(LiqPayAPI.InternetAcquiring.Invoice.Cancel.Request.t(), [
          {:private_key, String.t()}
          | {:base_url, String.t() | URI.t()}
          | {:client_pipeline, OpenAPIClient.Client.pipeline()}
        ]) ::
          {:ok, LiqPayAPI.InternetAcquiring.Invoice.Cancel.Response.t()}
          | {:error, OpenAPIClient.Client.Error.t()}
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
        {"application/json", {LiqPayAPI.InternetAcquiring.Invoice.Cancel.Request, :t}}
      ],
      response_types: [
        {200, [{"application/json", {LiqPayAPI.InternetAcquiring.Invoice.Cancel.Response, :t}}]}
      ]
    }
    |> OpenAPIClient.Client.Operation.put_private(
      __args__: initial_args,
      __call__: {__MODULE__, :cancel},
      __opts__: opts,
      __params__: [private_key: private_key],
      __profile__: :default
    )
    |> client.perform(client_pipeline)
  end

  @doc """
  Internet acquiring. Invoice. Issuing the invoice

  ## Arguments

    * `body`

  ## Options

    * `private_key`: Private key of the created company (not available to anyone except your developer). Default value obtained through a call to `Application.get_env(:liqpay_api_ex, :private_key)`
    * `base_url`: Request's base URL. Default value is taken from `@base_url`
    * `client_pipeline`: Client pipeline for making a request. Default value obtained through a call to `OpenAPIClient.Utils.get_config(__operation__, :client_pipeline)}

  """
  @spec issue(LiqPayAPI.InternetAcquiring.Invoice.Issue.Request.t(), [
          {:private_key, String.t()}
          | {:base_url, String.t() | URI.t()}
          | {:client_pipeline, OpenAPIClient.Client.pipeline()}
        ]) ::
          {:ok, LiqPayAPI.InternetAcquiring.Invoice.Issue.Response.t()}
          | {:error, OpenAPIClient.Client.Error.t()}
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
        {"application/json", {LiqPayAPI.InternetAcquiring.Invoice.Issue.Request, :t}}
      ],
      response_types: [
        {200, [{"application/json", {LiqPayAPI.InternetAcquiring.Invoice.Issue.Response, :t}}]}
      ]
    }
    |> OpenAPIClient.Client.Operation.put_private(
      __args__: initial_args,
      __call__: {__MODULE__, :issue},
      __opts__: opts,
      __params__: [private_key: private_key],
      __profile__: :default
    )
    |> client.perform(client_pipeline)
  end

  @doc """
  Internet acquiring. Invoice. 

  ## Arguments

    * `body`

  ## Options

    * `private_key`: Private key of the created company (not available to anyone except your developer). Default value obtained through a call to `Application.get_env(:liqpay_api_ex, :private_key)`
    * `base_url`: Request's base URL. Default value is taken from `@base_url`
    * `client_pipeline`: Client pipeline for making a request. Default value obtained through a call to `OpenAPIClient.Utils.get_config(__operation__, :client_pipeline)}

  """
  @spec units(LiqPayAPI.InternetAcquiring.Invoice.Units.Request.t(), [
          {:private_key, String.t()}
          | {:base_url, String.t() | URI.t()}
          | {:client_pipeline, OpenAPIClient.Client.pipeline()}
        ]) ::
          {:ok,
           LiqPayAPI.InternetAcquiring.Invoice.Units.Response.MultiLanguage.t()
           | LiqPayAPI.InternetAcquiring.Invoice.Units.Response.SingleLanguage.t()}
          | {:error, OpenAPIClient.Client.Error.t()}
  def units(body, opts \\ []) do
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
      ]
    }
    |> OpenAPIClient.Client.Operation.put_private(
      __args__: initial_args,
      __call__: {__MODULE__, :units},
      __opts__: opts,
      __params__: [private_key: private_key],
      __profile__: :default
    )
    |> client.perform(client_pipeline)
  end
end
