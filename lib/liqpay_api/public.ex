defmodule LiqPayAPI.Public do
  @moduledoc """
  Provides API endpoints related to public
  """

  @base_url "https://api.privatbank.ua"

  @doc """
  Public. Exchange rates archive

  ## Arguments

    * `date`: Exchange rate date

  ## Options

    * `json`: Default value is `true`
    * `base_url`: Request's base URL. Default value is taken from `@base_url`
    * `pipeline`: Operation pipeline for making a request. Default value obtained through a call to `OpenAPIClient.Utils.get_config(:default, :operation_pipeline)}
    * `client`: Module that implements `OpenAPIClient` behaviour. Default value obtained through a call to `OpenAPIClient.Utils.get_config(:default, :client, OpenAPIClient)`

  """
  @spec archive(Date.t()) ::
          {:ok, LiqPayAPI.Public.Archive.Response.t()} | {:error, OpenAPIClient.Error.t()}
  @spec archive(Date.t(), [
          {:json, true}
          | {:base_url, String.t() | URI.t()}
          | {:pipeline, OpenAPIClient.pipeline()}
          | {:client, module()}
        ]) :: {:ok, LiqPayAPI.Public.Archive.Response.t()} | {:error, OpenAPIClient.Error.t()}
  def archive(date, opts \\ []) do
    pipeline = opts[:pipeline] || OpenAPIClient.Utils.get_config(:default, :operation_pipeline)
    base_url = opts[:base_url] || @base_url
    client = opts[:client] || OpenAPIClient.Utils.get_config(:default, :client, OpenAPIClient)

    client.operation(
      %OpenAPIClient.State{
        request_base_url: base_url,
        request_path: "/p24api/exchange_rates",
        method: :get,
        request_parameter_types: [
          {{:date, :query}, {"date", {:string, "date-liqpay"}}},
          {{:json, :query}, {"json", {:enum, [true]}, true}}
        ],
        response_types: [{200, [{"application/json", {LiqPayAPI.Public.Archive.Response, :t}}]}],
        function_args: [date: date],
        function_call: {__MODULE__, :archive},
        function_opts: opts,
        profile: :default
      },
      pipeline
    )
  end

  @doc """
  Public. Accounting rate of NBU

  ## Arguments

    * `year`: The identifier of the year from which you want to display the «NBU Accounting Rate».
       The parameter takes meanings in the format `YYYY`

  ## Options

    * `base_url`: Request's base URL. Default value is taken from `@base_url`
    * `pipeline`: Operation pipeline for making a request. Default value obtained through a call to `OpenAPIClient.Utils.get_config(:default, :operation_pipeline)}
    * `client`: Module that implements `OpenAPIClient` behaviour. Default value obtained through a call to `OpenAPIClient.Utils.get_config(:default, :client, OpenAPIClient)`

  """
  @spec discount_rate(String.t()) ::
          {:ok, [LiqPayAPI.Public.DiscountRate.Response.t()]} | {:error, OpenAPIClient.Error.t()}
  @spec discount_rate(String.t(), [
          {:base_url, String.t() | URI.t()}
          | {:pipeline, OpenAPIClient.pipeline()}
          | {:client, module()}
        ]) ::
          {:ok, [LiqPayAPI.Public.DiscountRate.Response.t()]} | {:error, OpenAPIClient.Error.t()}
  def discount_rate(year, opts \\ []) do
    pipeline = opts[:pipeline] || OpenAPIClient.Utils.get_config(:default, :operation_pipeline)
    base_url = opts[:base_url] || "https://api.buh.privatbank.ua"
    client = opts[:client] || OpenAPIClient.Utils.get_config(:default, :client, OpenAPIClient)

    client.operation(
      %OpenAPIClient.State{
        request_base_url: base_url,
        request_path: "/ratenbu.php",
        method: :get,
        request_parameter_types: [{{:year, :query}, {"year", {:string, :generic}}}],
        response_types: [
          {200, [{"application/json", [{LiqPayAPI.Public.DiscountRate.Response, :t}]}]}
        ],
        function_args: [year: year],
        function_call: {__MODULE__, :discount_rate},
        function_opts: opts,
        profile: :default
      },
      pipeline
    )
  end

  @doc """
  Public. Exchange rates of PrivatBank

  ## Arguments

    * `coursid`: Possible values:
      * `5` - Cash rate of PrivatBank (in the branches)
      * `11` - Non-cash exchange rate of PrivatBank (conversion by cards, Privat24, replenishment of deposits)
      

  ## Options

    * `exchange`: Default value is `true`
    * `json`: Default value is `true`
    * `base_url`: Request's base URL. Default value is taken from `@base_url`
    * `pipeline`: Operation pipeline for making a request. Default value obtained through a call to `OpenAPIClient.Utils.get_config(:default, :operation_pipeline)}
    * `client`: Module that implements `OpenAPIClient` behaviour. Default value obtained through a call to `OpenAPIClient.Utils.get_config(:default, :client, OpenAPIClient)`

  """
  @spec exchange(:cash | :non_cash) ::
          {:ok, [LiqPayAPI.Public.Exchange.Response.t()]} | {:error, OpenAPIClient.Error.t()}
  @spec exchange(:cash | :non_cash, [
          {:exchange, true}
          | {:json, true}
          | {:base_url, String.t() | URI.t()}
          | {:pipeline, OpenAPIClient.pipeline()}
          | {:client, module()}
        ]) :: {:ok, [LiqPayAPI.Public.Exchange.Response.t()]} | {:error, OpenAPIClient.Error.t()}
  def exchange(coursid, opts \\ []) do
    pipeline = opts[:pipeline] || OpenAPIClient.Utils.get_config(:default, :operation_pipeline)
    base_url = opts[:base_url] || @base_url
    client = opts[:client] || OpenAPIClient.Utils.get_config(:default, :client, OpenAPIClient)

    client.operation(
      %OpenAPIClient.State{
        request_base_url: base_url,
        request_path: "/p24api/pubinfo",
        method: :get,
        request_parameter_types: [
          {{:coursid, :query}, {"coursid", {:enum, cash: 5, non_cash: 11}}},
          {{:exchange, :query}, {"exchange", {:enum, [true]}, true}},
          {{:json, :query}, {"json", {:enum, [true]}, true}}
        ],
        response_types: [
          {200, [{"application/json", [{LiqPayAPI.Public.Exchange.Response, :t}]}]}
        ],
        function_args: [coursid: coursid],
        function_call: {__MODULE__, :exchange},
        function_opts: opts,
        profile: :default
      },
      pipeline
    )
  end
end
