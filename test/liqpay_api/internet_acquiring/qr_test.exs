defmodule LiqPayAPI.InternetAcquiring.QrTest do
  use ExUnit.Case, async: true
  import Mox

  @httpoison OpenAPIClient.HTTPoisonMock
  @client OpenAPIClientMock

  setup :verify_on_exit!

  describe "static/2" do
    test "[200] performs a request, encodes QR.Static.Request from request's body and decodes QR.Static.Response from response's body" do
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
                  "action" => "staticQrCreate",
                  "amount" => 5.0,
                  "currency" => "USD",
                  "description" => "description text",
                  "final_date" => "string",
                  "order_id" => "order_id_1",
                  "public_key" => "string",
                  "server_url" => "http://example.com",
                  "version" => 3
                }} == Jason.decode(body)

        assert {:ok, body_encoded} =
                 Jason.encode(%{
                   "amount" => 1,
                   "create_date" => 1_732_879_229,
                   "currency" => "UAH",
                   "description" => "Test payment",
                   "final_date" => 1_706_750_625,
                   "id" => 22169,
                   "qrdata" => "dev_qr_1732715427100430_562_PyVh3SpRuKJuDjrBbfMF",
                   "shop_id" => 190_828,
                   "status" => "active",
                   "url" =>
                     "https://www.privat24.ua/rd/send_qr/liqpay_static_qr/dev_qr_1732715427100430_562_PyVh3SpRuKJuDjrBbfMF"
                 })

        {:ok,
         %HTTPoison.Response{
           status_code: 200,
           headers: [{"Content-Type", "application/json"}],
           body: body_encoded
         }}
      end)

      assert {:ok,
              %LiqPayAPI.InternetAcquiring.QR.Static.Response{
                amount: 1,
                create_date: ~U[2024-11-29 11:20:29Z],
                currency: "UAH",
                description: "Test payment",
                final_date: ~U[2024-02-01 01:23:45Z],
                id: 22169,
                qrdata: "dev_qr_1732715427100430_562_PyVh3SpRuKJuDjrBbfMF",
                shop_id: 190_828,
                status: "active",
                url:
                  "https://www.privat24.ua/rd/send_qr/liqpay_static_qr/dev_qr_1732715427100430_562_PyVh3SpRuKJuDjrBbfMF"
              }} ==
               LiqPayAPI.InternetAcquiring.Qr.static(
                 %LiqPayAPI.InternetAcquiring.QR.Static.Request{
                   action: :static_qr_create,
                   amount: 5.0,
                   currency: :usd,
                   description: "description text",
                   final_date: "string",
                   order_id: "order_id_1",
                   public_key: "string",
                   server_url: "http://example.com",
                   version: 3
                 },
                 private_key: "a4825234f4bae72a0be04eafe9e8e2bada209255",
                 base_url: "https://example.com"
               )
    end
  end

  describe "dynamic/2" do
    test "[200] performs a request, encodes QR.Dynamic.Request from request's body and decodes QR.Dynamic.Response from response's body" do
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
                  "action" => "payqr",
                  "amount" => 5.0,
                  "currency" => "USD",
                  "customer" => "string",
                  "dae" => "string",
                  "description" => "description text",
                  "info" => "External information for payments",
                  "ip" => "string",
                  "language" => "uk",
                  "order_id" => "order_id_1",
                  "prepare" => "1",
                  "product_category" => "string",
                  "product_description" => "string",
                  "product_name" => "string",
                  "product_url" => "http://example.com",
                  "public_key" => "string",
                  "recurringbytoken" => "1",
                  "server_url" => "http://example.com",
                  "split_rules" => [
                    %{
                      "amount" => 404,
                      "commission_payer" => "sender",
                      "description" => "string",
                      "public_key" => "i000000001",
                      "rro_info" => %{
                        "delivery_emails" => ["string"],
                        "items" => [
                          %{"amount" => 2, "cost" => 404, "id" => 123_456, "price" => 202}
                        ]
                      },
                      "server_url" => "https://server1/callback"
                    }
                  ],
                  "split_tickets_only" => true,
                  "version" => 3
                }} == Jason.decode(body)

        assert {:ok, body_encoded} =
                 Jason.encode(%{
                   "acq_id" => 414_963,
                   "action" => "payqr",
                   "agent_commission" => 0.0,
                   "amount" => 1.0,
                   "amount_bonus" => 0.0,
                   "amount_credit" => 38.02,
                   "amount_debit" => 38.02,
                   "commission_credit" => 0.57,
                   "commission_debit" => 0.0,
                   "create_date" => 1_706_094_696_741,
                   "currency" => "USD",
                   "currency_credit" => "UAH",
                   "currency_debit" => "UAH",
                   "description" => "description",
                   "end_date" => 1_706_750_625_987,
                   "ip" => "string",
                   "is_3ds" => true,
                   "liqpay_order_id" => "JW2R3SHX1706094696737441",
                   "mpi_eci" => 7,
                   "order_id" => "pay_by_QR_code_015",
                   "payment_id" => 2_418_368_618,
                   "public_key" => "sandbox_i63492854596",
                   "qr_code" =>
                     "https://www.privat24.ua/rd/send_qr/liqpay_static_qr/checkoutpayqr_1706094696772917_51089507_p2LbqaPtrVodh6Yv4q2R",
                   "receiver_commission" => 0.02,
                   "result" => "ok",
                   "sender_bonus" => 0.0,
                   "sender_commission" => 0.0,
                   "status" => "wait_qr",
                   "transaction_id" => 2_418_368_618,
                   "type" => "buy",
                   "version" => 3
                 })

        {:ok,
         %HTTPoison.Response{
           status_code: 200,
           headers: [{"Content-Type", "application/json"}],
           body: body_encoded
         }}
      end)

      assert {:ok,
              %LiqPayAPI.InternetAcquiring.QR.Dynamic.Response{
                acq_id: 414_963,
                action: "payqr",
                agent_commission: 0.0,
                amount: 1.0,
                amount_bonus: 0.0,
                amount_credit: 38.02,
                amount_debit: 38.02,
                commission_credit: 0.57,
                commission_debit: 0.0,
                create_date: ~U[2024-01-24 11:11:36.741Z],
                currency: "USD",
                currency_credit: "UAH",
                currency_debit: "UAH",
                description: "description",
                end_date: ~U[2024-02-01 01:23:45.987Z],
                ip: "string",
                is_3ds: true,
                liqpay_order_id: "JW2R3SHX1706094696737441",
                mpi_eci: 7,
                order_id: "pay_by_QR_code_015",
                payment_id: 2_418_368_618,
                public_key: "sandbox_i63492854596",
                qr_code:
                  "https://www.privat24.ua/rd/send_qr/liqpay_static_qr/checkoutpayqr_1706094696772917_51089507_p2LbqaPtrVodh6Yv4q2R",
                receiver_commission: 0.02,
                result: :ok,
                sender_bonus: 0.0,
                sender_commission: 0.0,
                status: :wait_qr,
                transaction_id: 2_418_368_618,
                type: "buy",
                version: 3
              }} ==
               LiqPayAPI.InternetAcquiring.Qr.dynamic(
                 %LiqPayAPI.InternetAcquiring.QR.Dynamic.Request{
                   action: :payqr,
                   amount: 5.0,
                   currency: :usd,
                   customer: "string",
                   dae: "string",
                   description: "description text",
                   info: "External information for payments",
                   ip: "string",
                   language: :uk,
                   order_id: "order_id_1",
                   prepare: true,
                   product_category: "string",
                   product_description: "string",
                   product_name: "string",
                   product_url: "http://example.com",
                   public_key: "string",
                   recurringbytoken: true,
                   server_url: "http://example.com",
                   split_rules: [
                     %LiqPayAPI.InternetAcquiring.QR.Dynamic.Request.SplitRules{
                       amount: 404,
                       commission_payer: :sender,
                       description: "string",
                       public_key: "i000000001",
                       rro_info:
                         %LiqPayAPI.InternetAcquiring.QR.Dynamic.Request.SplitRulesRroInfo{
                           delivery_emails: ["string"],
                           items: [
                             %LiqPayAPI.InternetAcquiring.QR.Dynamic.Request.SplitRulesRroInfoItems{
                               amount: 2,
                               cost: 404,
                               id: 123_456,
                               price: 202
                             }
                           ]
                         },
                       server_url: "https://server1/callback"
                     }
                   ],
                   split_tickets_only: true,
                   version: 3
                 },
                 private_key: "a4825234f4bae72a0be04eafe9e8e2bada209255",
                 base_url: "https://example.com"
               )
    end
  end
end
