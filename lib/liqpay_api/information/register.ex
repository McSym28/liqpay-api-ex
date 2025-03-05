defmodule LiqPayAPI.Information.Register do
  @moduledoc """
  Provides API endpoints related to information/register
  """

  @base_url "https://www.liqpay.ua"

  @doc """
  Informational. Registers. Compensation once a day

  ## Arguments

    * `body`

  ## Options

    * `private_key`: Private key of the created company (not available to anyone except your developer). Default value obtained through a call to `Application.get_env(:liqpay_api_ex, :private_key)`
    * `base_url`: Request's base URL. Default value is taken from `@base_url`
    * `client_pipeline`: Client pipeline for making a request. Default value obtained through a call to `OpenAPIClient.Utils.get_config(__operation__, :client_pipeline)}

  """
  @spec compensation_per_day(LiqPayAPI.Information.Register.CompensationPerDay.Request.t(), [
          {:private_key, String.t()}
          | {:base_url, String.t() | URI.t()}
          | {:client_pipeline, OpenAPIClient.Client.pipeline()}
        ]) ::
          {:ok, LiqPayAPI.Information.Register.CompensationPerDay.Response.t()}
          | {:error, OpenAPIClient.Client.Error.t()}
  def compensation_per_day(body, opts \\ []) do
    body =
      case body do
        %LiqPayAPI.Information.Register.CompensationPerDay.Request{public_key: nil} ->
          %LiqPayAPI.Information.Register.CompensationPerDay.Request{
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
        {"application/json", {LiqPayAPI.Information.Register.CompensationPerDay.Request, :t}}
      ],
      response_types: [
        {200,
         [{"application/json", {LiqPayAPI.Information.Register.CompensationPerDay.Response, :t}}]}
      ]
    }
    |> OpenAPIClient.Client.Operation.put_private(
      __args__: initial_args,
      __call__: {__MODULE__, :compensation_per_day},
      __opts__: opts,
      __params__: [private_key: private_key],
      __profile__: :default
    )
    |> client.perform(client_pipeline)
  end

  @doc """
  Informational. Registers. Compensation per transaction

  ## Arguments

    * `body`

  ## Options

    * `private_key`: Private key of the created company (not available to anyone except your developer). Default value obtained through a call to `Application.get_env(:liqpay_api_ex, :private_key)`
    * `base_url`: Request's base URL. Default value is taken from `@base_url`
    * `client_pipeline`: Client pipeline for making a request. Default value obtained through a call to `OpenAPIClient.Utils.get_config(__operation__, :client_pipeline)}

  """
  @spec compensation_per_transaction(
          LiqPayAPI.Information.Register.CompensationPerTransaction.Request.t(),
          [
            {:private_key, String.t()}
            | {:base_url, String.t() | URI.t()}
            | {:client_pipeline, OpenAPIClient.Client.pipeline()}
          ]
        ) ::
          {:ok, LiqPayAPI.Information.Register.CompensationPerTransaction.Response.t()}
          | {:error, OpenAPIClient.Client.Error.t()}
  def compensation_per_transaction(body, opts \\ []) do
    body =
      case body do
        %LiqPayAPI.Information.Register.CompensationPerTransaction.Request{public_key: nil} ->
          %LiqPayAPI.Information.Register.CompensationPerTransaction.Request{
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
        {"application/json",
         {LiqPayAPI.Information.Register.CompensationPerTransaction.Request, :t}}
      ],
      response_types: [
        {200,
         [
           {"application/json",
            {LiqPayAPI.Information.Register.CompensationPerTransaction.Response, :t}}
         ]}
      ]
    }
    |> OpenAPIClient.Client.Operation.put_private(
      __args__: initial_args,
      __call__: {__MODULE__, :compensation_per_transaction},
      __opts__: opts,
      __params__: [private_key: private_key],
      __profile__: :default
    )
    |> client.perform(client_pipeline)
  end

  @doc """
  Informational. Registers. Getting the compensation registry

  ## Arguments

    * `body`

  ## Options

    * `private_key`: Private key of the created company (not available to anyone except your developer). Default value obtained through a call to `Application.get_env(:liqpay_api_ex, :private_key)`
    * `base_url`: Request's base URL. Default value is taken from `@base_url`
    * `client_pipeline`: Client pipeline for making a request. Default value obtained through a call to `OpenAPIClient.Utils.get_config(__operation__, :client_pipeline)}

  """
  @spec compensation_report(LiqPayAPI.Information.Register.CompensationReport.Request.t(), [
          {:private_key, String.t()}
          | {:base_url, String.t() | URI.t()}
          | {:client_pipeline, OpenAPIClient.Client.pipeline()}
        ]) ::
          {:ok, LiqPayAPI.Information.Register.CompensationReport.Response.t()}
          | {:error, OpenAPIClient.Client.Error.t()}
  def compensation_report(body, opts \\ []) do
    body =
      case body do
        %LiqPayAPI.Information.Register.CompensationReport.Request{public_key: nil} ->
          %LiqPayAPI.Information.Register.CompensationReport.Request{
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
        {"application/json", {LiqPayAPI.Information.Register.CompensationReport.Request, :t}}
      ],
      response_types: [
        {200,
         [{"application/json", {LiqPayAPI.Information.Register.CompensationReport.Response, :t}}]}
      ]
    }
    |> OpenAPIClient.Client.Operation.put_private(
      __args__: initial_args,
      __call__: {__MODULE__, :compensation_report},
      __opts__: opts,
      __params__: [private_key: private_key],
      __profile__: :default
    )
    |> client.perform(client_pipeline)
  end

  @doc """
  Informational. Registers. Registry by p2p operation

  ## Arguments

    * `body`

  ## Options

    * `private_key`: Private key of the created company (not available to anyone except your developer). Default value obtained through a call to `Application.get_env(:liqpay_api_ex, :private_key)`
    * `base_url`: Request's base URL. Default value is taken from `@base_url`
    * `client_pipeline`: Client pipeline for making a request. Default value obtained through a call to `OpenAPIClient.Utils.get_config(__operation__, :client_pipeline)}

  """
  @spec compensation_report_p2p(
          LiqPayAPI.Information.Register.CompensationReportP2P.Request.t(),
          [
            {:private_key, String.t()}
            | {:base_url, String.t() | URI.t()}
            | {:client_pipeline, OpenAPIClient.Client.pipeline()}
          ]
        ) ::
          {:ok, LiqPayAPI.Information.Register.CompensationReportP2P.Response.t()}
          | {:error, OpenAPIClient.Client.Error.t()}
  def compensation_report_p2p(body, opts \\ []) do
    body =
      case body do
        %LiqPayAPI.Information.Register.CompensationReportP2P.Request{public_key: nil} ->
          %LiqPayAPI.Information.Register.CompensationReportP2P.Request{
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
        {"application/json", {LiqPayAPI.Information.Register.CompensationReportP2P.Request, :t}}
      ],
      response_types: [
        {200,
         [
           {"application/json",
            {LiqPayAPI.Information.Register.CompensationReportP2P.Response, :t}}
         ]}
      ]
    }
    |> OpenAPIClient.Client.Operation.put_private(
      __args__: initial_args,
      __call__: {__MODULE__, :compensation_report_p2p},
      __opts__: opts,
      __params__: [private_key: private_key],
      __profile__: :default
    )
    |> client.perform(client_pipeline)
  end

  @doc """
  Informational. Registers. p2p operation registry status

  ## Arguments

    * `body`

  ## Options

    * `private_key`: Private key of the created company (not available to anyone except your developer). Default value obtained through a call to `Application.get_env(:liqpay_api_ex, :private_key)`
    * `base_url`: Request's base URL. Default value is taken from `@base_url`
    * `client_pipeline`: Client pipeline for making a request. Default value obtained through a call to `OpenAPIClient.Utils.get_config(__operation__, :client_pipeline)}

  """
  @spec compensation_report_p2p_status(
          LiqPayAPI.Information.Register.CompensationReportP2PStatus.Request.t(),
          [
            {:private_key, String.t()}
            | {:base_url, String.t() | URI.t()}
            | {:client_pipeline, OpenAPIClient.Client.pipeline()}
          ]
        ) ::
          {:ok, LiqPayAPI.Information.Register.CompensationReportP2PStatus.Response.t()}
          | {:error, OpenAPIClient.Client.Error.t()}
  def compensation_report_p2p_status(body, opts \\ []) do
    body =
      case body do
        %LiqPayAPI.Information.Register.CompensationReportP2PStatus.Request{public_key: nil} ->
          %LiqPayAPI.Information.Register.CompensationReportP2PStatus.Request{
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
        {"application/json",
         {LiqPayAPI.Information.Register.CompensationReportP2PStatus.Request, :t}}
      ],
      response_types: [
        {200,
         [
           {"application/json",
            {LiqPayAPI.Information.Register.CompensationReportP2PStatus.Response, :t}}
         ]}
      ]
    }
    |> OpenAPIClient.Client.Operation.put_private(
      __args__: initial_args,
      __call__: {__MODULE__, :compensation_report_p2p_status},
      __opts__: opts,
      __params__: [private_key: private_key],
      __profile__: :default
    )
    |> client.perform(client_pipeline)
  end

  @doc """
  Informational. Registers. Getting the compensation registry status

  ## Arguments

    * `body`

  ## Options

    * `private_key`: Private key of the created company (not available to anyone except your developer). Default value obtained through a call to `Application.get_env(:liqpay_api_ex, :private_key)`
    * `base_url`: Request's base URL. Default value is taken from `@base_url`
    * `client_pipeline`: Client pipeline for making a request. Default value obtained through a call to `OpenAPIClient.Utils.get_config(__operation__, :client_pipeline)}

  """
  @spec compensation_report_status(
          LiqPayAPI.Information.Register.CompensationReportStatus.Request.t(),
          [
            {:private_key, String.t()}
            | {:base_url, String.t() | URI.t()}
            | {:client_pipeline, OpenAPIClient.Client.pipeline()}
          ]
        ) ::
          {:ok, LiqPayAPI.Information.Register.CompensationReportStatus.Response.t()}
          | {:error, OpenAPIClient.Client.Error.t()}
  def compensation_report_status(body, opts \\ []) do
    body =
      case body do
        %LiqPayAPI.Information.Register.CompensationReportStatus.Request{public_key: nil} ->
          %LiqPayAPI.Information.Register.CompensationReportStatus.Request{
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
        {"application/json",
         {LiqPayAPI.Information.Register.CompensationReportStatus.Request, :t}}
      ],
      response_types: [
        {200,
         [
           {"application/json",
            {LiqPayAPI.Information.Register.CompensationReportStatus.Response, :t}}
         ]}
      ]
    }
    |> OpenAPIClient.Client.Operation.put_private(
      __args__: initial_args,
      __call__: {__MODULE__, :compensation_report_status},
      __opts__: opts,
      __params__: [private_key: private_key],
      __profile__: :default
    )
    |> client.perform(client_pipeline)
  end
end
