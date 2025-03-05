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
    * `client_pipeline`: Client pipeline for making a request. Default value obtained through a call to `OpenAPIClient.Utils.get_config(__operation__, :client_pipeline)}

  """
  @spec archive(Date.t(), [
          {:json, true}
          | {:base_url, String.t() | URI.t()}
          | {:client_pipeline, OpenAPIClient.Client.pipeline()}
        ]) ::
          {:ok, LiqPayAPI.Public.Archive.Response.t()} | {:error, OpenAPIClient.Client.Error.t()}
  def archive(date, opts \\ []) do
    initial_args = [date: date]

    client_pipeline = Keyword.get(opts, :client_pipeline)
    base_url = opts[:base_url] || @base_url

    typed_encoder =
      OpenAPIClient.Utils.get_config(:default, :typed_encoder, OpenAPIClient.Client.TypedEncoder)

    {:ok, date} =
      typed_encoder.encode(
        date,
        {:string, "date-liqpay"},
        [{:parameter, :query, "date"}, {"/p24api/exchange_rates", :get}],
        typed_encoder
      )

    json = Keyword.get_lazy(opts, :json, fn -> true end)
    query_params = %{"date" => date, "json" => json}
    client = OpenAPIClient.Utils.get_config(:default, :client, OpenAPIClient.Client)

    %OpenAPIClient.Client.Operation{
      request_base_url: base_url,
      request_url: "/p24api/exchange_rates",
      request_method: :get,
      request_query_params: query_params,
      response_types: [{200, [{"application/json", {LiqPayAPI.Public.Archive.Response, :t}}]}]
    }
    |> OpenAPIClient.Client.Operation.put_private(
      __args__: initial_args,
      __call__: {__MODULE__, :archive},
      __opts__: opts,
      __profile__: :default
    )
    |> client.perform(client_pipeline)
  end

  @doc """
  Public. Accounting rate of NBU

  ## Arguments

    * `year`: The identifier of the year from which you want to display the «NBU Accounting Rate».
       The parameter takes meanings in the format `YYYY`

  ## Options

    * `base_url`: Request's base URL. Default value is taken from `@base_url`
    * `client_pipeline`: Client pipeline for making a request. Default value obtained through a call to `OpenAPIClient.Utils.get_config(__operation__, :client_pipeline)}

  """
  @spec discount_rate(String.t(), [
          {:base_url, String.t() | URI.t()} | {:client_pipeline, OpenAPIClient.Client.pipeline()}
        ]) ::
          {:ok, [LiqPayAPI.Public.DiscountRate.Response.t()]}
          | {:error, OpenAPIClient.Client.Error.t()}
  def discount_rate(year, opts \\ []) do
    initial_args = [year: year]

    client_pipeline = Keyword.get(opts, :client_pipeline)
    base_url = opts[:base_url] || "https://api.buh.privatbank.ua"
    query_params = %{"year" => year}
    client = OpenAPIClient.Utils.get_config(:default, :client, OpenAPIClient.Client)

    %OpenAPIClient.Client.Operation{
      request_base_url: base_url,
      request_url: "/ratenbu.php",
      request_method: :get,
      request_query_params: query_params,
      response_types: [
        {200, [{"application/json", [{LiqPayAPI.Public.DiscountRate.Response, :t}]}]}
      ]
    }
    |> OpenAPIClient.Client.Operation.put_private(
      __args__: initial_args,
      __call__: {__MODULE__, :discount_rate},
      __opts__: opts,
      __profile__: :default
    )
    |> client.perform(client_pipeline)
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
    * `client_pipeline`: Client pipeline for making a request. Default value obtained through a call to `OpenAPIClient.Utils.get_config(__operation__, :client_pipeline)}

  """
  @spec exchange(:cash | :non_cash, [
          {:exchange, true}
          | {:json, true}
          | {:base_url, String.t() | URI.t()}
          | {:client_pipeline, OpenAPIClient.Client.pipeline()}
        ]) ::
          {:ok, [LiqPayAPI.Public.Exchange.Response.t()]}
          | {:error, OpenAPIClient.Client.Error.t()}
  def exchange(coursid, opts \\ []) do
    initial_args = [coursid: coursid]

    client_pipeline = Keyword.get(opts, :client_pipeline)
    base_url = opts[:base_url] || @base_url

    typed_encoder =
      OpenAPIClient.Utils.get_config(:default, :typed_encoder, OpenAPIClient.Client.TypedEncoder)

    {:ok, coursid} =
      typed_encoder.encode(
        coursid,
        {:enum, cash: 5, non_cash: 11},
        [{:parameter, :query, "coursid"}, {"/p24api/pubinfo", :get}],
        typed_encoder
      )

    exchange = Keyword.get_lazy(opts, :exchange, fn -> true end)
    json = Keyword.get_lazy(opts, :json, fn -> true end)
    query_params = %{"coursid" => coursid, "exchange" => exchange, "json" => json}
    client = OpenAPIClient.Utils.get_config(:default, :client, OpenAPIClient.Client)

    %OpenAPIClient.Client.Operation{
      request_base_url: base_url,
      request_url: "/p24api/pubinfo",
      request_method: :get,
      request_query_params: query_params,
      response_types: [{200, [{"application/json", [{LiqPayAPI.Public.Exchange.Response, :t}]}]}]
    }
    |> OpenAPIClient.Client.Operation.put_private(
      __args__: initial_args,
      __call__: {__MODULE__, :exchange},
      __opts__: opts,
      __profile__: :default
    )
    |> client.perform(client_pipeline)
  end
end
