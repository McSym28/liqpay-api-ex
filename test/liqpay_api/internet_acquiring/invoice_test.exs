defmodule LiqPayAPI.InternetAcquiring.InvoiceTest do
  use ExUnit.Case, async: true
  import Mox

  @httpoison OpenAPIClient.HTTPoisonMock
  @client OpenAPIClientMock

  setup :verify_on_exit!

  describe "cancel/2" do
    test "[200] performs a request, encodes Cancel.Request from request's body and decodes Cancel.Response from response's body" do
      expect(@client, :operation, fn state, pipeline ->
        assert {:ok, "a4825234f4bae72a0be04eafe9e8e2bada209255"} ==
                 Keyword.fetch(state.function_opts, :private_key)

        OpenAPIClient.operation(state, pipeline)
      end)

      expect(@httpoison, :request, fn :post,
                                      "https://example.com/api/request",
                                      body,
                                      headers,
                                      _ ->
        assert {:ok, "application/x-www-form-urlencoded"} ==
                 OpenAPIClient.Utils.get_content_type(headers)

        form_data = URI.decode_query(body)
        assert {:ok, signature} = Map.fetch(form_data, "signature")
        assert {:ok, data} = Map.fetch(form_data, "data")

        assert LiqPayAPI.Client.Signature.check?(
                 data,
                 "a4825234f4bae72a0be04eafe9e8e2bada209255",
                 signature
               )

        assert {:ok, body} = Base.decode64(data)
        headers = List.keystore(headers, "content-type", 0, {"content-type", "application/json"})
        assert {:ok, "application/json"} == OpenAPIClient.Utils.get_content_type(headers)

        assert {:ok,
                %{
                  "action" => "invoice_cancel",
                  "order_id" => "order_id_1",
                  "public_key" => "string",
                  "version" => 3
                }} == Jason.decode(body)

        assert {:ok, body_encoded} = Jason.encode(%{"invoice_id" => 6173, "result" => "ok"})

        {:ok,
         %HTTPoison.Response{
           status_code: 200,
           headers: [{"Content-Type", "application/json"}],
           body: body_encoded
         }}
      end)

      assert {:ok,
              %LiqPayAPI.InternetAcquiring.Invoice.Cancel.Response{invoice_id: 6173, result: :ok}} ==
               LiqPayAPI.InternetAcquiring.Invoice.cancel(
                 %LiqPayAPI.InternetAcquiring.Invoice.Cancel.Request{
                   action: :invoice_cancel,
                   order_id: "order_id_1",
                   public_key: "string",
                   version: 3
                 },
                 private_key: "a4825234f4bae72a0be04eafe9e8e2bada209255",
                 base_url: "https://example.com"
               )
    end
  end

  describe "units/2" do
    test "[200] performs a request and encodes Units.Request from request's body" do
      expect(@client, :operation, fn state, pipeline ->
        assert {:ok, "a4825234f4bae72a0be04eafe9e8e2bada209255"} ==
                 Keyword.fetch(state.function_opts, :private_key)

        OpenAPIClient.operation(state, pipeline)
      end)

      expect(@httpoison, :request, fn :post,
                                      "https://example.com/api/request",
                                      body,
                                      headers,
                                      _ ->
        assert {:ok, "application/x-www-form-urlencoded"} ==
                 OpenAPIClient.Utils.get_content_type(headers)

        form_data = URI.decode_query(body)
        assert {:ok, signature} = Map.fetch(form_data, "signature")
        assert {:ok, data} = Map.fetch(form_data, "data")

        assert LiqPayAPI.Client.Signature.check?(
                 data,
                 "a4825234f4bae72a0be04eafe9e8e2bada209255",
                 signature
               )

        assert {:ok, body} = Base.decode64(data)
        headers = List.keystore(headers, "content-type", 0, {"content-type", "application/json"})
        assert {:ok, "application/json"} == OpenAPIClient.Utils.get_content_type(headers)

        assert {:ok,
                %{
                  "action" => "invoice_units_get_list",
                  "hide_name_lang" => true,
                  "language" => "uk",
                  "version" => 3
                }} == Jason.decode(body)

        assert {:ok, body_encoded} =
                 Jason.encode(%{
                   "full_name_en" => "string",
                   "full_name_uk" => "string",
                   "id" => 1,
                   "short_name_en" => "string",
                   "short_name_uk" => "string"
                 })

        {:ok,
         %HTTPoison.Response{
           status_code: 200,
           headers: [{"Content-Type", "application/json"}],
           body: body_encoded
         }}
      end)

      assert {:ok,
              %LiqPayAPI.InternetAcquiring.Invoice.Units.Response.MultiLanguage{
                full_name_en: "string",
                full_name_uk: "string",
                id: 1,
                short_name_en: "string",
                short_name_uk: "string"
              }} ==
               LiqPayAPI.InternetAcquiring.Invoice.units(
                 %LiqPayAPI.InternetAcquiring.Invoice.Units.Request{
                   action: :invoice_units_get_list,
                   hide_name_lang: true,
                   language: :uk,
                   version: 3
                 },
                 private_key: "a4825234f4bae72a0be04eafe9e8e2bada209255",
                 base_url: "https://example.com"
               )
    end
  end

  describe "issue/2" do
    test "[200] performs a request, encodes Issue.Request from request's body and decodes Issue.Response from response's body" do
      expect(@client, :operation, fn state, pipeline ->
        assert {:ok, "a4825234f4bae72a0be04eafe9e8e2bada209255"} ==
                 Keyword.fetch(state.function_opts, :private_key)

        OpenAPIClient.operation(state, pipeline)
      end)

      expect(@httpoison, :request, fn :post,
                                      "https://example.com/api/request",
                                      body,
                                      headers,
                                      _ ->
        assert {:ok, "application/x-www-form-urlencoded"} ==
                 OpenAPIClient.Utils.get_content_type(headers)

        form_data = URI.decode_query(body)
        assert {:ok, signature} = Map.fetch(form_data, "signature")
        assert {:ok, data} = Map.fetch(form_data, "data")

        assert LiqPayAPI.Client.Signature.check?(
                 data,
                 "a4825234f4bae72a0be04eafe9e8e2bada209255",
                 signature
               )

        assert {:ok, body} = Base.decode64(data)
        headers = List.keystore(headers, "content-type", 0, {"content-type", "application/json"})
        assert {:ok, "application/json"} == OpenAPIClient.Utils.get_content_type(headers)

        assert {:ok,
                %{
                  "action" => "invoice_send",
                  "action_payment" => "hold",
                  "amount" => 5.0,
                  "currency" => "USD",
                  "description" => "string",
                  "email" => "client-email@gmail.com",
                  "expired_date" => "2024-01-02 01:23:45",
                  "goods" => [%{"amount" => 1, "count" => 2, "name" => "USB", "unit" => "pcs."}],
                  "language" => "uk",
                  "order_id" => "order_id_1",
                  "phone" => "+380950000001",
                  "public_key" => "string",
                  "result_url" => "http://example.com",
                  "rro_info" => %{
                    "delivery_emails" => ["string"],
                    "items" => [%{"amount" => 2, "cost" => 404, "id" => 123_456, "price" => 202}]
                  },
                  "server_url" => "http://example.com",
                  "version" => 3
                }} == Jason.decode(body)

        assert {:ok, body_encoded} =
                 Jason.encode(%{
                   "action" => "pay",
                   "amount" => 0.03,
                   "currency" => "UAH",
                   "description" => "Invoice",
                   "href" =>
                     "https://liqpay.ua/apipay/invoice/150112455893001_6480_GBe3Mxwnq2O7G0MLX4gkQLtZf2",
                   "id" => 6173,
                   "order_id" => "98R1U1OV1485849059893399",
                   "receiver_type" => "email",
                   "receiver_value" => "test@gmail.com",
                   "result" => "ok",
                   "status" => "invoice_wait",
                   "token" => "150112455893001_6480_GBe3Mxwnq2O7G0MLX4gkQLtZf2"
                 })

        {:ok,
         %HTTPoison.Response{
           status_code: 200,
           headers: [{"Content-Type", "application/json"}],
           body: body_encoded
         }}
      end)

      assert {:ok,
              %LiqPayAPI.InternetAcquiring.Invoice.Issue.Response{
                action: :pay,
                amount: 0.03,
                currency: "UAH",
                description: "Invoice",
                href:
                  "https://liqpay.ua/apipay/invoice/150112455893001_6480_GBe3Mxwnq2O7G0MLX4gkQLtZf2",
                id: 6173,
                order_id: "98R1U1OV1485849059893399",
                receiver_type: "email",
                receiver_value: "test@gmail.com",
                result: :ok,
                status: :invoice_wait,
                token: "150112455893001_6480_GBe3Mxwnq2O7G0MLX4gkQLtZf2"
              }} ==
               LiqPayAPI.InternetAcquiring.Invoice.issue(
                 %LiqPayAPI.InternetAcquiring.Invoice.Issue.Request{
                   action: :invoice_send,
                   action_payment: :hold,
                   amount: 5.0,
                   currency: :usd,
                   description: "string",
                   email: "client-email@gmail.com",
                   expired_date: ~U[2024-01-02 01:23:45Z],
                   goods: [
                     %LiqPayAPI.InternetAcquiring.Invoice.Issue.Request.Goods{
                       amount: 1,
                       count: 2,
                       name: "USB",
                       unit: "pcs."
                     }
                   ],
                   language: :uk,
                   order_id: "order_id_1",
                   phone: "+380950000001",
                   public_key: "string",
                   result_url: "http://example.com",
                   rro_info: %LiqPayAPI.InternetAcquiring.Invoice.Issue.Request.RroInfo{
                     delivery_emails: ["string"],
                     items: [
                       %LiqPayAPI.InternetAcquiring.Invoice.Issue.Request.RroInfoItems{
                         amount: 2,
                         cost: 404,
                         id: 123_456,
                         price: 202
                       }
                     ]
                   },
                   server_url: "http://example.com",
                   version: 3
                 },
                 private_key: "a4825234f4bae72a0be04eafe9e8e2bada209255",
                 base_url: "https://example.com"
               )
    end
  end
end
