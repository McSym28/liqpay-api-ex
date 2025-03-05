defmodule LiqPayAPI.PublicTest do
  use ExUnit.Case, async: true
  import Mox

  @httpoison OpenAPIClient.HTTPoisonMock
  @client OpenAPIClient.ClientMock

  setup :verify_on_exit!

  describe "exchange/2" do
    test "[200] performs a request and encodes array of Exchange.Response from response's body" do
      expect(@client, :perform, &OpenAPIClient.Client.perform/2)

      expect(@httpoison, :request, fn :get, "https://example.com/p24api/pubinfo", _, _, options ->
        assert {_, 5} = List.keyfind(options[:params], "coursid", 0)
        assert {_, true} = List.keyfind(options[:params], "exchange", 0)
        assert {_, true} = List.keyfind(options[:params], "json", 0)

        assert {:ok, body_encoded} =
                 Jason.encode([
                   %{
                     "base_ccy" => "UAH",
                     "buy" => "19.20000",
                     "ccy" => "EUR",
                     "sale" => "20.00000"
                   }
                 ])

        {:ok,
         %HTTPoison.Response{
           status_code: 200,
           headers: [{"Content-Type", "application/json"}],
           body: body_encoded
         }}
      end)

      assert {:ok,
              [
                %LiqPayAPI.Public.Exchange.Response{
                  base_ccy: "UAH",
                  buy: "19.20000",
                  ccy: "EUR",
                  sale: "20.00000"
                }
              ]} ==
               LiqPayAPI.Public.exchange(:cash,
                 json: true,
                 exchange: true,
                 base_url: "https://example.com"
               )
    end
  end

  describe "archive/2" do
    test "[200] performs a request and encodes Archive.Response from response's body" do
      expect(@client, :perform, &OpenAPIClient.Client.perform/2)

      expect(@httpoison, :request, fn :get,
                                      "https://example.com/p24api/exchange_rates",
                                      _,
                                      _,
                                      options ->
        assert {_, "01.02.2024"} = List.keyfind(options[:params], "date", 0)
        assert {_, true} = List.keyfind(options[:params], "json", 0)

        assert {:ok, body_encoded} =
                 Jason.encode(%{
                   "bank" => "PB",
                   "baseCurrency" => 980,
                   "baseCurrencyLit" => "UAH",
                   "date" => "01.12.2014",
                   "exchangeRate" => [
                     %{
                       "baseCurrency" => "UAH",
                       "currency" => "CHF",
                       "purchaseRate" => 15.5,
                       "purchaseRateNB" => 15.638975,
                       "saleRate" => 17.0,
                       "saleRateNB" => 15.638975
                     }
                   ]
                 })

        {:ok,
         %HTTPoison.Response{
           status_code: 200,
           headers: [{"Content-Type", "application/json"}],
           body: body_encoded
         }}
      end)

      assert {:ok,
              %LiqPayAPI.Public.Archive.Response{
                bank: "PB",
                base_currency: 980,
                base_currency_lit: "UAH",
                date: ~D[2014-12-01],
                exchange_rate: [
                  %LiqPayAPI.Public.Archive.Response.ExchangeRate{
                    base_currency: "UAH",
                    currency: "CHF",
                    purchase_rate: 15.5,
                    purchase_rate_nb: 15.638975,
                    sale_rate: 17.0,
                    sale_rate_nb: 15.638975
                  }
                ]
              }} ==
               LiqPayAPI.Public.archive(~D[2024-02-01],
                 json: true,
                 base_url: "https://example.com"
               )
    end
  end

  describe "discount_rate/2" do
    test "[200] performs a request and encodes array of DiscountRate.Response from response's body" do
      expect(@client, :perform, &OpenAPIClient.Client.perform/2)

      expect(@httpoison, :request, fn :get, "https://example.com/ratenbu.php", _, _, options ->
        assert {_, "string"} = List.keyfind(options[:params], "year", 0)

        assert {:ok, body_encoded} =
                 Jason.encode([%{"rate_date" => "17.07.2014", "rate_value" => 12.5}])

        {:ok,
         %HTTPoison.Response{
           status_code: 200,
           headers: [{"Content-Type", "application/json"}],
           body: body_encoded
         }}
      end)

      assert {:ok,
              [
                %LiqPayAPI.Public.DiscountRate.Response{
                  rate_date: ~D[2014-07-17],
                  rate_value: 12.5
                }
              ]} == LiqPayAPI.Public.discount_rate("string", base_url: "https://example.com")
    end
  end
end
