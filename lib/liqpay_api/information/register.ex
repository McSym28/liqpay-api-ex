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

    * `base_url`: Request's base URL. Default value is taken from `@base_url`
    * `pipeline`: Operation pipeline for making a request. Default value obtained through a call to `OpenAPIClient.Utils.get_config(:default, :operation_pipeline)}
    * `client`: Module that implements `OpenAPIClient` behaviour. Default value obtained through a call to `OpenAPIClient.Utils.get_config(:default, :client, OpenAPIClient)`

  """
  @spec compensation_per_day(LiqPayAPI.Information.Register.CompensationPerDay.Request.t()) ::
          {:ok, LiqPayAPI.Information.Register.CompensationPerDay.Response.t()}
          | {:error, OpenAPIClient.Error.t()}
  @spec compensation_per_day(LiqPayAPI.Information.Register.CompensationPerDay.Request.t(), [
          {:base_url, String.t() | URI.t()}
          | {:pipeline, OpenAPIClient.pipeline()}
          | {:client, module()}
        ]) ::
          {:ok, LiqPayAPI.Information.Register.CompensationPerDay.Response.t()}
          | {:error, OpenAPIClient.Error.t()}
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

    pipeline = opts[:pipeline] || OpenAPIClient.Utils.get_config(:default, :operation_pipeline)
    base_url = opts[:base_url] || @base_url
    client = opts[:client] || OpenAPIClient.Utils.get_config(:default, :client, OpenAPIClient)

    client.operation(
      %OpenAPIClient.State{
        request_base_url: base_url,
        request_path: "/api/request",
        method: :post,
        request_types: [
          {"application/json", {LiqPayAPI.Information.Register.CompensationPerDay.Request, :t}}
        ],
        response_types: [
          {200,
           [
             {"application/json",
              {LiqPayAPI.Information.Register.CompensationPerDay.Response, :t}}
           ]}
        ],
        function_args: [body: body],
        function_call: {__MODULE__, :compensation_per_day},
        function_opts: opts,
        profile: :default
      },
      pipeline
    )
  end

  @doc """
  Informational. Registers. Compensation per transaction

  ## Arguments

    * `body`

  ## Options

    * `base_url`: Request's base URL. Default value is taken from `@base_url`
    * `pipeline`: Operation pipeline for making a request. Default value obtained through a call to `OpenAPIClient.Utils.get_config(:default, :operation_pipeline)}
    * `client`: Module that implements `OpenAPIClient` behaviour. Default value obtained through a call to `OpenAPIClient.Utils.get_config(:default, :client, OpenAPIClient)`

  """
  @spec compensation_per_transaction(
          LiqPayAPI.Information.Register.CompensationPerTransaction.Request.t()
        ) ::
          {:ok, LiqPayAPI.Information.Register.CompensationPerTransaction.Response.t()}
          | {:error, OpenAPIClient.Error.t()}
  @spec compensation_per_transaction(
          LiqPayAPI.Information.Register.CompensationPerTransaction.Request.t(),
          [
            {:base_url, String.t() | URI.t()}
            | {:pipeline, OpenAPIClient.pipeline()}
            | {:client, module()}
          ]
        ) ::
          {:ok, LiqPayAPI.Information.Register.CompensationPerTransaction.Response.t()}
          | {:error, OpenAPIClient.Error.t()}
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

    pipeline = opts[:pipeline] || OpenAPIClient.Utils.get_config(:default, :operation_pipeline)
    base_url = opts[:base_url] || @base_url
    client = opts[:client] || OpenAPIClient.Utils.get_config(:default, :client, OpenAPIClient)

    client.operation(
      %OpenAPIClient.State{
        request_base_url: base_url,
        request_path: "/api/request",
        method: :post,
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
        ],
        function_args: [body: body],
        function_call: {__MODULE__, :compensation_per_transaction},
        function_opts: opts,
        profile: :default
      },
      pipeline
    )
  end

  @doc """
  Informational. Registers. Getting the compensation registry

  ## Arguments

    * `body`

  ## Options

    * `base_url`: Request's base URL. Default value is taken from `@base_url`
    * `pipeline`: Operation pipeline for making a request. Default value obtained through a call to `OpenAPIClient.Utils.get_config(:default, :operation_pipeline)}
    * `client`: Module that implements `OpenAPIClient` behaviour. Default value obtained through a call to `OpenAPIClient.Utils.get_config(:default, :client, OpenAPIClient)`

  """
  @spec compensation_report(LiqPayAPI.Information.Register.CompensationReport.Request.t()) ::
          {:ok, LiqPayAPI.Information.Register.CompensationReport.Response.t()}
          | {:error, OpenAPIClient.Error.t()}
  @spec compensation_report(LiqPayAPI.Information.Register.CompensationReport.Request.t(), [
          {:base_url, String.t() | URI.t()}
          | {:pipeline, OpenAPIClient.pipeline()}
          | {:client, module()}
        ]) ::
          {:ok, LiqPayAPI.Information.Register.CompensationReport.Response.t()}
          | {:error, OpenAPIClient.Error.t()}
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

    pipeline = opts[:pipeline] || OpenAPIClient.Utils.get_config(:default, :operation_pipeline)
    base_url = opts[:base_url] || @base_url
    client = opts[:client] || OpenAPIClient.Utils.get_config(:default, :client, OpenAPIClient)

    client.operation(
      %OpenAPIClient.State{
        request_base_url: base_url,
        request_path: "/api/request",
        method: :post,
        request_types: [
          {"application/json", {LiqPayAPI.Information.Register.CompensationReport.Request, :t}}
        ],
        response_types: [
          {200,
           [
             {"application/json",
              {LiqPayAPI.Information.Register.CompensationReport.Response, :t}}
           ]}
        ],
        function_args: [body: body],
        function_call: {__MODULE__, :compensation_report},
        function_opts: opts,
        profile: :default
      },
      pipeline
    )
  end

  @doc """
  Informational. Registers. Registry by p2p operation

  ## Arguments

    * `body`

  ## Options

    * `base_url`: Request's base URL. Default value is taken from `@base_url`
    * `pipeline`: Operation pipeline for making a request. Default value obtained through a call to `OpenAPIClient.Utils.get_config(:default, :operation_pipeline)}
    * `client`: Module that implements `OpenAPIClient` behaviour. Default value obtained through a call to `OpenAPIClient.Utils.get_config(:default, :client, OpenAPIClient)`

  """
  @spec compensation_report_p2p(LiqPayAPI.Information.Register.CompensationReportP2P.Request.t()) ::
          {:ok, LiqPayAPI.Information.Register.CompensationReportP2P.Response.t()}
          | {:error, OpenAPIClient.Error.t()}
  @spec compensation_report_p2p(
          LiqPayAPI.Information.Register.CompensationReportP2P.Request.t(),
          [
            {:base_url, String.t() | URI.t()}
            | {:pipeline, OpenAPIClient.pipeline()}
            | {:client, module()}
          ]
        ) ::
          {:ok, LiqPayAPI.Information.Register.CompensationReportP2P.Response.t()}
          | {:error, OpenAPIClient.Error.t()}
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

    pipeline = opts[:pipeline] || OpenAPIClient.Utils.get_config(:default, :operation_pipeline)
    base_url = opts[:base_url] || @base_url
    client = opts[:client] || OpenAPIClient.Utils.get_config(:default, :client, OpenAPIClient)

    client.operation(
      %OpenAPIClient.State{
        request_base_url: base_url,
        request_path: "/api/request",
        method: :post,
        request_types: [
          {"application/json", {LiqPayAPI.Information.Register.CompensationReportP2P.Request, :t}}
        ],
        response_types: [
          {200,
           [
             {"application/json",
              {LiqPayAPI.Information.Register.CompensationReportP2P.Response, :t}}
           ]}
        ],
        function_args: [body: body],
        function_call: {__MODULE__, :compensation_report_p2p},
        function_opts: opts,
        profile: :default
      },
      pipeline
    )
  end

  @doc """
  Informational. Registers. p2p operation registry status

  ## Arguments

    * `body`

  ## Options

    * `base_url`: Request's base URL. Default value is taken from `@base_url`
    * `pipeline`: Operation pipeline for making a request. Default value obtained through a call to `OpenAPIClient.Utils.get_config(:default, :operation_pipeline)}
    * `client`: Module that implements `OpenAPIClient` behaviour. Default value obtained through a call to `OpenAPIClient.Utils.get_config(:default, :client, OpenAPIClient)`

  """
  @spec compensation_report_p2p_status(
          LiqPayAPI.Information.Register.CompensationReportP2PStatus.Request.t()
        ) ::
          {:ok, LiqPayAPI.Information.Register.CompensationReportP2PStatus.Response.t()}
          | {:error, OpenAPIClient.Error.t()}
  @spec compensation_report_p2p_status(
          LiqPayAPI.Information.Register.CompensationReportP2PStatus.Request.t(),
          [
            {:base_url, String.t() | URI.t()}
            | {:pipeline, OpenAPIClient.pipeline()}
            | {:client, module()}
          ]
        ) ::
          {:ok, LiqPayAPI.Information.Register.CompensationReportP2PStatus.Response.t()}
          | {:error, OpenAPIClient.Error.t()}
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

    pipeline = opts[:pipeline] || OpenAPIClient.Utils.get_config(:default, :operation_pipeline)
    base_url = opts[:base_url] || @base_url
    client = opts[:client] || OpenAPIClient.Utils.get_config(:default, :client, OpenAPIClient)

    client.operation(
      %OpenAPIClient.State{
        request_base_url: base_url,
        request_path: "/api/request",
        method: :post,
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
        ],
        function_args: [body: body],
        function_call: {__MODULE__, :compensation_report_p2p_status},
        function_opts: opts,
        profile: :default
      },
      pipeline
    )
  end

  @doc """
  Informational. Registers. Getting the compensation registry status

  ## Arguments

    * `body`

  ## Options

    * `base_url`: Request's base URL. Default value is taken from `@base_url`
    * `pipeline`: Operation pipeline for making a request. Default value obtained through a call to `OpenAPIClient.Utils.get_config(:default, :operation_pipeline)}
    * `client`: Module that implements `OpenAPIClient` behaviour. Default value obtained through a call to `OpenAPIClient.Utils.get_config(:default, :client, OpenAPIClient)`

  """
  @spec compensation_report_status(
          LiqPayAPI.Information.Register.CompensationReportStatus.Request.t()
        ) ::
          {:ok, LiqPayAPI.Information.Register.CompensationReportStatus.Response.t()}
          | {:error, OpenAPIClient.Error.t()}
  @spec compensation_report_status(
          LiqPayAPI.Information.Register.CompensationReportStatus.Request.t(),
          [
            {:base_url, String.t() | URI.t()}
            | {:pipeline, OpenAPIClient.pipeline()}
            | {:client, module()}
          ]
        ) ::
          {:ok, LiqPayAPI.Information.Register.CompensationReportStatus.Response.t()}
          | {:error, OpenAPIClient.Error.t()}
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

    pipeline = opts[:pipeline] || OpenAPIClient.Utils.get_config(:default, :operation_pipeline)
    base_url = opts[:base_url] || @base_url
    client = opts[:client] || OpenAPIClient.Utils.get_config(:default, :client, OpenAPIClient)

    client.operation(
      %OpenAPIClient.State{
        request_base_url: base_url,
        request_path: "/api/request",
        method: :post,
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
        ],
        function_args: [body: body],
        function_call: {__MODULE__, :compensation_report_status},
        function_opts: opts,
        profile: :default
      },
      pipeline
    )
  end
end
