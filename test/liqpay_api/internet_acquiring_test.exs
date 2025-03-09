defmodule LiqPayAPI.InternetAcquiringTest do
  use ExUnit.Case, async: true
  import Mox

  @httpoison OpenAPIClient.HTTPoisonMock
  @client OpenAPIClientMock

  setup :verify_on_exit!

  describe "cash/2" do
    test "[200] performs a request, encodes Cash.Request from request's body and decodes Cash.Response from response's body" do
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
                  "action" => "paycash",
                  "amount" => 5.0,
                  "currency" => "USD",
                  "customer" => "string",
                  "dae" => "string",
                  "description" => "description text",
                  "expired_date" => "2024-01-02 01:23:45",
                  "info" => "External information for payments",
                  "ip" => "string",
                  "language" => "uk",
                  "order_id" => "order_id_1",
                  "phone" => "+380950000001",
                  "prepare" => "1",
                  "product_category" => "string",
                  "product_description" => "string",
                  "product_name" => "string",
                  "product_url" => "http://example.com",
                  "public_key" => "string",
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
                  "version" => 3
                }} == Jason.decode(body)

        assert {:ok, body_encoded} =
                 Jason.encode(%{
                   "acq_id" => 414_963,
                   "action" => "paycash",
                   "agent_commission" => 0.0,
                   "amount" => 1.0,
                   "amount_bonus" => 0.35,
                   "amount_credit" => 1.0,
                   "amount_debit" => 1.0,
                   "commission_credit" => 0.0,
                   "commission_debit" => 0.0,
                   "confirm_phone" => "380933454182",
                   "create_date" => 1_715_335_274_831,
                   "currency" => "UAH",
                   "currency_credit" => "UAH",
                   "currency_debit" => "UAH",
                   "description" => "description text",
                   "end_date" => 1_501_684_842_777,
                   "ip" => "string",
                   "is_3ds" => true,
                   "liqpay_order_id" => "YP0L4E5S1715335274827674",
                   "mpi_eci" => 7,
                   "order_id" => "order_id_45_pay_by_cash1",
                   "payment_id" => 13_291_496,
                   "paytype" => "cash",
                   "public_key" => "i16202663459",
                   "receiver_commission" => 0.0,
                   "result" => "ok",
                   "sender_bonus" => 0.35,
                   "sender_commission" => 0.0,
                   "sender_first_name" => "Tetiana",
                   "sender_last_name" => "Stanko",
                   "sender_phone" => "380933454182",
                   "status" => "cash_wait",
                   "transaction_id" => 13_291_496,
                   "type" => "cash",
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
              %LiqPayAPI.InternetAcquiring.Cash.Response{
                acq_id: 414_963,
                action: "paycash",
                agent_commission: 0.0,
                amount: 1.0,
                amount_bonus: 0.35,
                amount_credit: 1.0,
                amount_debit: 1.0,
                commission_credit: 0.0,
                commission_debit: 0.0,
                confirm_phone: "380933454182",
                create_date: ~U[2024-05-10 10:01:14.831Z],
                currency: "UAH",
                currency_credit: "UAH",
                currency_debit: "UAH",
                description: "description text",
                end_date: ~U[2017-08-02 14:40:42.777Z],
                ip: "string",
                is_3ds: true,
                liqpay_order_id: "YP0L4E5S1715335274827674",
                mpi_eci: 7,
                order_id: "order_id_45_pay_by_cash1",
                payment_id: 13_291_496,
                paytype: :cash,
                public_key: "i16202663459",
                receiver_commission: 0.0,
                result: :ok,
                sender_bonus: 0.35,
                sender_commission: 0.0,
                sender_first_name: "Tetiana",
                sender_last_name: "Stanko",
                sender_phone: "380933454182",
                status: :cash_wait,
                transaction_id: 13_291_496,
                type: "cash",
                version: 3
              }} ==
               LiqPayAPI.InternetAcquiring.cash(
                 %LiqPayAPI.InternetAcquiring.Cash.Request{
                   action: :paycash,
                   amount: 5.0,
                   currency: :usd,
                   customer: "string",
                   dae: "string",
                   description: "description text",
                   expired_date: ~U[2024-01-02 01:23:45Z],
                   info: "External information for payments",
                   ip: "string",
                   language: :uk,
                   order_id: "order_id_1",
                   phone: "+380950000001",
                   prepare: true,
                   product_category: "string",
                   product_description: "string",
                   product_name: "string",
                   product_url: "http://example.com",
                   public_key: "string",
                   server_url: "http://example.com",
                   split_rules: [
                     %LiqPayAPI.InternetAcquiring.Cash.Request.SplitRules{
                       amount: 404,
                       commission_payer: :sender,
                       description: "string",
                       public_key: "i000000001",
                       rro_info: %LiqPayAPI.InternetAcquiring.Cash.Request.SplitRulesRroInfo{
                         delivery_emails: ["string"],
                         items: [
                           %LiqPayAPI.InternetAcquiring.Cash.Request.SplitRulesRroInfoItems{
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
                   version: 3
                 },
                 private_key: "a4825234f4bae72a0be04eafe9e8e2bada209255",
                 base_url: "https://example.com"
               )
    end
  end

  describe "card_payment/2" do
    test "[200] performs a request, encodes CardPayment.Request from request's body and decodes CardPayment.Response from response's body" do
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
                  "action" => "pay",
                  "amount" => 5.0,
                  "card" => "4731195301524634",
                  "card_cvv" => "111",
                  "card_exp_month" => "08",
                  "card_exp_year" => "19",
                  "currency" => "USD",
                  "customer" => "string",
                  "dae" => "string",
                  "description" => "description text",
                  "info" => "External information for payments",
                  "ip" => "string",
                  "language" => "uk",
                  "order_id" => "order_id_1",
                  "paytype" => "apay",
                  "phone" => "+380950000001",
                  "prepare" => "1",
                  "product_category" => "string",
                  "product_description" => "string",
                  "product_name" => "string",
                  "product_url" => "http://example.com",
                  "public_key" => "string",
                  "recurring" => true,
                  "recurringbytoken" => "1",
                  "result_url" => "http://example.com",
                  "rro_info" => %{
                    "delivery_emails" => ["string"],
                    "items" => [%{"amount" => 2, "cost" => 404, "id" => 123_456, "price" => 202}]
                  },
                  "sender_address" => "string",
                  "sender_city" => "string",
                  "sender_country_code" => "string",
                  "sender_email" => "string",
                  "sender_first_name" => "string",
                  "sender_last_name" => "string",
                  "sender_postal_code" => "string",
                  "sender_shipping_state" => "string",
                  "sender_state" => "string",
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
                  "subscribe" => "1",
                  "subscribe_date_start" => "2024-01-02 01:23:45",
                  "subscribe_periodicity" => "day",
                  "tavv" => "string",
                  "tid" => "string",
                  "version" => 3
                }} == Jason.decode(body)

        assert {:ok, body_encoded} =
                 Jason.encode(%{
                   "acq_id" => 414_963,
                   "action" => "pay",
                   "agent_commission" => 0.0,
                   "amount" => 100.0,
                   "amount_bonus" => 0.0,
                   "amount_credit" => 100.0,
                   "amount_debit" => 100.0,
                   "authcode_credit" => "329007",
                   "authcode_debit" => "388000",
                   "bonus_procent" => 0.0,
                   "bonus_type" => "bonusplus",
                   "card_token" => "CDRES215658546306B200061FCC53A86B",
                   "commission_credit" => 0.0,
                   "commission_debit" => 0.0,
                   "confirm_phone" => "380680375936",
                   "create_date" => 1_501_685_446_633,
                   "currency" => "UAH",
                   "currency_credit" => "UAH",
                   "currency_debit" => "UAH",
                   "dcc_allowed" => [
                     %{
                       "amount" => 3.5984,
                       "commission" => 0.0,
                       "currency" => "USD",
                       "rate" => 27.7905
                     }
                   ],
                   "description" => "description",
                   "end_date" => 1_501_685_446_633,
                   "ip" => "8.8.8.8",
                   "is_3ds" => true,
                   "language" => "uk",
                   "liqpay_order_id" => "NYMK3AE61501685438251925",
                   "mpi_cres" => "string",
                   "mpi_eci" => 7,
                   "order_id" => "98R1U1OV1485849059893399",
                   "payment_id" => 165_172,
                   "paytype" => "card",
                   "public_key" => "i000000000",
                   "receiver_commission" => 0.0,
                   "result" => "ok",
                   "rrn_credit" => "000663747003",
                   "rrn_debit" => "000663747000",
                   "sender_bonus" => 0.0,
                   "sender_card_bank" => "pb",
                   "sender_card_country" => 804,
                   "sender_card_mask2" => "473118*50",
                   "sender_card_type" => "visa",
                   "sender_commission" => 0.0,
                   "sender_first_name" => "first_name",
                   "sender_last_name" => "last_name",
                   "sender_phone" => "380950000001",
                   "status" => "success",
                   "transaction_id" => 165_172,
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
              %LiqPayAPI.InternetAcquiring.CardPayment.Response{
                acq_id: 414_963,
                action: :pay,
                agent_commission: 0.0,
                amount: 100.0,
                amount_bonus: 0.0,
                amount_credit: 100.0,
                amount_debit: 100.0,
                authcode_credit: "329007",
                authcode_debit: "388000",
                bonus_procent: 0.0,
                bonus_type: :bonusplus,
                card_token: "CDRES215658546306B200061FCC53A86B",
                commission_credit: 0.0,
                commission_debit: 0.0,
                confirm_phone: "380680375936",
                create_date: ~U[2017-08-02 14:50:46.633Z],
                currency: "UAH",
                currency_credit: "UAH",
                currency_debit: "UAH",
                dcc_allowed: [
                  %LiqPayAPI.InternetAcquiring.CardPayment.Response.DCCAllowed{
                    amount: 3.5984,
                    commission: 0.0,
                    currency: "USD",
                    rate: 27.7905
                  }
                ],
                description: "description",
                end_date: ~U[2017-08-02 14:50:46.633Z],
                ip: "8.8.8.8",
                is_3ds: true,
                language: "uk",
                liqpay_order_id: "NYMK3AE61501685438251925",
                mpi_cres: "string",
                mpi_eci: 7,
                order_id: "98R1U1OV1485849059893399",
                payment_id: 165_172,
                paytype: :card,
                public_key: "i000000000",
                receiver_commission: 0.0,
                result: :ok,
                rrn_credit: "000663747003",
                rrn_debit: "000663747000",
                sender_bonus: 0.0,
                sender_card_bank: "pb",
                sender_card_country: 804,
                sender_card_mask2: "473118*50",
                sender_card_type: "visa",
                sender_commission: 0.0,
                sender_first_name: "first_name",
                sender_last_name: "last_name",
                sender_phone: "380950000001",
                status: :success,
                transaction_id: 165_172,
                type: "buy",
                version: 3
              }} ==
               LiqPayAPI.InternetAcquiring.card_payment(
                 %LiqPayAPI.InternetAcquiring.CardPayment.Request{
                   action: :pay,
                   amount: 5.0,
                   card: "4731195301524634",
                   card_cvv: "111",
                   card_exp_month: "08",
                   card_exp_year: "19",
                   currency: :usd,
                   customer: "string",
                   dae: "string",
                   description: "description text",
                   info: "External information for payments",
                   ip: "string",
                   language: :uk,
                   order_id: "order_id_1",
                   paytype: :apay,
                   phone: "+380950000001",
                   prepare: true,
                   product_category: "string",
                   product_description: "string",
                   product_name: "string",
                   product_url: "http://example.com",
                   public_key: "string",
                   recurring: true,
                   recurringbytoken: true,
                   regular_payment:
                     %LiqPayAPI.InternetAcquiring.CardPayment.Request.RegularPayment{
                       subscribe: :"1",
                       subscribe_date_start: ~U[2024-01-02 01:23:45Z],
                       subscribe_periodicity: :day
                     },
                   result_url: "http://example.com",
                   rro_info: %LiqPayAPI.InternetAcquiring.CardPayment.Request.RROInfo{
                     delivery_emails: ["string"],
                     items: [
                       %LiqPayAPI.InternetAcquiring.CardPayment.Request.RROInfo.Items{
                         amount: 2,
                         cost: 404,
                         id: 123_456,
                         price: 202
                       }
                     ]
                   },
                   sender: %LiqPayAPI.InternetAcquiring.CardPayment.Request.Sender{
                     sender_address: "string",
                     sender_city: "string",
                     sender_country_code: "string",
                     sender_email: "string",
                     sender_first_name: "string",
                     sender_last_name: "string",
                     sender_postal_code: "string",
                     sender_shipping_state: "string",
                     sender_state: "string"
                   },
                   server_url: "http://example.com",
                   split_rules: [
                     %LiqPayAPI.InternetAcquiring.CardPayment.Request.SplitRules{
                       amount: 404,
                       commission_payer: :sender,
                       description: "string",
                       public_key: "i000000001",
                       rro_info:
                         %LiqPayAPI.InternetAcquiring.CardPayment.Request.SplitRulesRroInfo{
                           delivery_emails: ["string"],
                           items: [
                             %LiqPayAPI.InternetAcquiring.CardPayment.Request.SplitRulesRroInfoItems{
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
                   tavv: "string",
                   tid: "string",
                   version: 3
                 },
                 private_key: "a4825234f4bae72a0be04eafe9e8e2bada209255",
                 base_url: "https://example.com"
               )
    end
  end

  describe "token/2" do
    test "[200] performs a request, encodes Token.Request from request's body and decodes Token.Response from response's body" do
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
                  "action" => "pay",
                  "amount" => 5.0,
                  "card_token" => "B5BВB0D00B88B00ED00A00D0D",
                  "currency" => "USD",
                  "customer" => "string",
                  "dae" => "string",
                  "description" => "description text",
                  "info" => "External information for payments",
                  "ip" => "string",
                  "is_recurring" => "false",
                  "language" => "uk",
                  "order_id" => "order_id_1",
                  "phone" => "+380950000001",
                  "prepare" => "1",
                  "product_category" => "string",
                  "product_description" => "string",
                  "product_name" => "string",
                  "product_url" => "http://example.com",
                  "public_key" => "string",
                  "sender_address" => "string",
                  "sender_city" => "string",
                  "sender_country_code" => "string",
                  "sender_first_name" => "string",
                  "sender_last_name" => "string",
                  "sender_postal_code" => "string",
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
                   "action" => "pay",
                   "agent_commission" => 0.0,
                   "amount" => 0.2,
                   "amount_bonus" => 0.01,
                   "amount_credit" => 0.2,
                   "amount_debit" => 0.2,
                   "authcode_credit" => "628292",
                   "authcode_debit" => "642291",
                   "bonus_procent" => 5.0,
                   "bonus_type" => "promo",
                   "card_token" => "811D840858969FD9DB4DF7040161BD1C324C983C",
                   "commission_credit" => 0.0,
                   "commission_debit" => 0.0,
                   "confirm_phone" => "380933454182",
                   "create_date" => 1_717_058_255_167,
                   "currency" => "UAH",
                   "currency_credit" => "UAH",
                   "currency_debit" => "UAH",
                   "description" => "Pay by token",
                   "end_date" => 1_717_058_257_356,
                   "ip" => "0.0.0.0",
                   "is_3ds" => true,
                   "language" => "uk",
                   "liqpay_order_id" => "V7NCWJBH1717058255159399",
                   "mpi_eci" => 7,
                   "order_id" => "id446789m",
                   "payment_id" => 13_293_344,
                   "paytype" => "token",
                   "public_key" => "i16202663459",
                   "receiver_commission" => 0.0,
                   "result" => "ok",
                   "rrn_credit" => "000000468437",
                   "rrn_debit" => "000000468436",
                   "sender_bonus" => 0.01,
                   "sender_card_bank" => "pb",
                   "sender_card_country" => 804,
                   "sender_card_mask2" => "545708*24",
                   "sender_card_type" => "mc",
                   "sender_commission" => 0.0,
                   "sender_first_name" => "Maria",
                   "sender_last_name" => "GHenm",
                   "sender_phone" => "380933454182",
                   "status" => "success",
                   "transaction_id" => 13_293_344,
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
              %LiqPayAPI.InternetAcquiring.Token.Response{
                acq_id: 414_963,
                action: :pay,
                agent_commission: 0.0,
                amount: 0.2,
                amount_bonus: 0.01,
                amount_credit: 0.2,
                amount_debit: 0.2,
                authcode_credit: "628292",
                authcode_debit: "642291",
                bonus_procent: 5.0,
                bonus_type: :promo,
                card_token: "811D840858969FD9DB4DF7040161BD1C324C983C",
                commission_credit: 0.0,
                commission_debit: 0.0,
                confirm_phone: "380933454182",
                create_date: ~U[2024-05-30 08:37:35.167Z],
                currency: "UAH",
                currency_credit: "UAH",
                currency_debit: "UAH",
                description: "Pay by token",
                end_date: ~U[2024-05-30 08:37:37.356Z],
                ip: "0.0.0.0",
                is_3ds: true,
                language: "uk",
                liqpay_order_id: "V7NCWJBH1717058255159399",
                mpi_eci: 7,
                order_id: "id446789m",
                payment_id: 13_293_344,
                paytype: "token",
                public_key: "i16202663459",
                receiver_commission: 0.0,
                result: :ok,
                rrn_credit: "000000468437",
                rrn_debit: "000000468436",
                sender_bonus: 0.01,
                sender_card_bank: "pb",
                sender_card_country: 804,
                sender_card_mask2: "545708*24",
                sender_card_type: "mc",
                sender_commission: 0.0,
                sender_first_name: "Maria",
                sender_last_name: "GHenm",
                sender_phone: "380933454182",
                status: :success,
                transaction_id: 13_293_344,
                type: "buy",
                version: 3
              }} ==
               LiqPayAPI.InternetAcquiring.token(
                 %LiqPayAPI.InternetAcquiring.Token.Request{
                   action: :pay,
                   amount: 5.0,
                   card_token: "B5BВB0D00B88B00ED00A00D0D",
                   currency: :usd,
                   customer: "string",
                   dae: "string",
                   description: "description text",
                   info: "External information for payments",
                   ip: "string",
                   is_recurring: false,
                   language: :uk,
                   order_id: "order_id_1",
                   phone: "+380950000001",
                   prepare: true,
                   product_category: "string",
                   product_description: "string",
                   product_name: "string",
                   product_url: "http://example.com",
                   public_key: "string",
                   sender: %LiqPayAPI.InternetAcquiring.Token.Request.Sender{
                     sender_address: "string",
                     sender_city: "string",
                     sender_country_code: "string",
                     sender_first_name: "string",
                     sender_last_name: "string",
                     sender_postal_code: "string"
                   },
                   server_url: "http://example.com",
                   split_rules: [
                     %LiqPayAPI.InternetAcquiring.Token.Request.SplitRules{
                       amount: 404,
                       commission_payer: :sender,
                       description: "string",
                       public_key: "i000000001",
                       rro_info: %LiqPayAPI.InternetAcquiring.Token.Request.SplitRulesRroInfo{
                         delivery_emails: ["string"],
                         items: [
                           %LiqPayAPI.InternetAcquiring.Token.Request.SplitRulesRroInfoItems{
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

  describe "refund/2" do
    test "[200] performs a request, encodes Refund.Request from request's body and decodes Refund.Response from response's body" do
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
                  "action" => "refund",
                  "amount" => 5.0,
                  "order_id" => "order_id_1",
                  "public_key" => "string",
                  "version" => 3
                }} == Jason.decode(body)

        assert {:ok, body_encoded} =
                 Jason.encode(%{
                   "action" => "hold",
                   "payment_id" => 2_417_662_437,
                   "result" => "ok",
                   "status" => "reversed",
                   "wait_amount" => true
                 })

        {:ok,
         %HTTPoison.Response{
           status_code: 200,
           headers: [{"Content-Type", "application/json"}],
           body: body_encoded
         }}
      end)

      assert {:ok,
              %LiqPayAPI.InternetAcquiring.Refund.Response{
                action: :hold,
                payment_id: 2_417_662_437,
                result: :ok,
                status: :reversed,
                wait_amount: true
              }} ==
               LiqPayAPI.InternetAcquiring.refund(
                 %LiqPayAPI.InternetAcquiring.Refund.Request{
                   action: :refund,
                   amount: 5.0,
                   order_id: "order_id_1",
                   public_key: "string",
                   version: 3
                 },
                 private_key: "a4825234f4bae72a0be04eafe9e8e2bada209255",
                 base_url: "https://example.com"
               )
    end
  end

  describe "dcc/2" do
    test "[200] performs a request, encodes DCC.Request from request's body and decodes DCC.Response from response's body" do
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
                  "action" => "pay",
                  "amount" => 5.0,
                  "card" => "string",
                  "card_cvv" => "string",
                  "card_exp_month" => "08",
                  "card_exp_year" => "19",
                  "card_token" => "B5BВB0D00B88B00ED00A00D0D",
                  "currency" => "EUR",
                  "description" => "string",
                  "is_dcc_debit" => true,
                  "language" => "uk",
                  "order_id" => "string",
                  "phone" => "+380950000001",
                  "prepare" => "tariffs",
                  "public_key" => "string",
                  "recurringbytoken" => "1",
                  "result_url" => "http://example.com",
                  "sandbox" => "1",
                  "sender_address" => "string",
                  "sender_city" => "string",
                  "sender_country_code" => "string",
                  "sender_first_name" => "string",
                  "sender_last_name" => "string",
                  "sender_postal_code" => "string",
                  "server_url" => "http://example.com",
                  "version" => 3
                }} == Jason.decode(body)

        assert {:ok, body_encoded} =
                 Jason.encode(%{
                   "acq_id" => 414_963,
                   "action" => "pay",
                   "agent_commission" => 0.0,
                   "amount" => 1.0,
                   "amount_bonus" => 5.98,
                   "amount_credit" => 463.01,
                   "amount_debit" => 463.01,
                   "authcode_credit" => "329007",
                   "authcode_debit" => "731230",
                   "bonus_procent" => 15.0,
                   "bonus_type" => "promo",
                   "card_token" => "AD002516D2E36F022951C401889E523B33DB04C7",
                   "commission_credit" => 0.0,
                   "commission_debit" => 0.0,
                   "confirm_phone" => "380933454182",
                   "create_date" => 1_715_080_189_406,
                   "currency" => "USD",
                   "currency_credit" => "UAH",
                   "currency_debit" => "UAH",
                   "dcc_allowed" => [
                     %{
                       "amount" => 10.8407,
                       "commission" => 0.0,
                       "currency" => "EUR",
                       "rate" => 42.710195
                     }
                   ],
                   "description" => "test_pay_googlePay",
                   "end_date" => 1_501_685_446_633,
                   "ip" => "8.8.8.8",
                   "is_3ds" => true,
                   "language" => "uk",
                   "liqpay_order_id" => "JH1W3RSE1715080189403270",
                   "mpi_cres" => "string",
                   "mpi_eci" => 7,
                   "order_id" => "id_gp7yxus776eг",
                   "payment_id" => 13_289_654,
                   "paytype" => "token",
                   "public_key" => "i95726270088",
                   "receiver_commission" => 4.86,
                   "result" => "ok",
                   "rrn_credit" => "000663747003",
                   "rrn_debit" => "000000451646",
                   "sender_bonus" => 0.15,
                   "sender_card_bank" => "pb",
                   "sender_card_country" => 804,
                   "sender_card_mask2" => "545708*24",
                   "sender_card_type" => "mc",
                   "sender_commission" => 0.0,
                   "sender_first_name" => "First name",
                   "sender_last_name" => "Last name",
                   "sender_phone" => "380846927406",
                   "status" => "wait_accept",
                   "transaction_id" => 13_289_654,
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
              %LiqPayAPI.InternetAcquiring.DCC.Response{
                acq_id: 414_963,
                action: :pay,
                agent_commission: 0.0,
                amount: 1.0,
                amount_bonus: 5.98,
                amount_credit: 463.01,
                amount_debit: 463.01,
                authcode_credit: "329007",
                authcode_debit: "731230",
                bonus_procent: 15.0,
                bonus_type: :promo,
                card_token: "AD002516D2E36F022951C401889E523B33DB04C7",
                commission_credit: 0.0,
                commission_debit: 0.0,
                confirm_phone: "380933454182",
                create_date: ~U[2024-05-07 11:09:49.406Z],
                currency: "USD",
                currency_credit: "UAH",
                currency_debit: "UAH",
                dcc_allowed: [
                  %LiqPayAPI.InternetAcquiring.DCC.Response.DCCAllowed{
                    amount: 10.8407,
                    commission: 0.0,
                    currency: "EUR",
                    rate: 42.710195
                  }
                ],
                description: "test_pay_googlePay",
                end_date: ~U[2017-08-02 14:50:46.633Z],
                ip: "8.8.8.8",
                is_3ds: true,
                language: "uk",
                liqpay_order_id: "JH1W3RSE1715080189403270",
                mpi_cres: "string",
                mpi_eci: 7,
                order_id: "id_gp7yxus776eг",
                payment_id: 13_289_654,
                paytype: "token",
                public_key: "i95726270088",
                receiver_commission: 4.86,
                result: :ok,
                rrn_credit: "000663747003",
                rrn_debit: "000000451646",
                sender_bonus: 0.15,
                sender_card_bank: "pb",
                sender_card_country: 804,
                sender_card_mask2: "545708*24",
                sender_card_type: "mc",
                sender_commission: 0.0,
                sender_first_name: "First name",
                sender_last_name: "Last name",
                sender_phone: "380846927406",
                status: :wait_accept,
                transaction_id: 13_289_654,
                type: "buy",
                version: 3
              }} ==
               LiqPayAPI.InternetAcquiring.dcc(
                 %LiqPayAPI.InternetAcquiring.DCC.Request{
                   action: :pay,
                   amount: 5.0,
                   card: "string",
                   card_cvv: "string",
                   card_exp_month: "08",
                   card_exp_year: "19",
                   card_token: "B5BВB0D00B88B00ED00A00D0D",
                   currency: :eur,
                   description: "string",
                   is_dcc_debit: true,
                   language: :uk,
                   order_id: "string",
                   phone: "+380950000001",
                   prepare: :tariffs,
                   public_key: "string",
                   recurringbytoken: true,
                   result_url: "http://example.com",
                   sandbox: true,
                   sender: %LiqPayAPI.InternetAcquiring.DCC.Request.Sender{
                     sender_address: "string",
                     sender_city: "string",
                     sender_country_code: "string",
                     sender_first_name: "string",
                     sender_last_name: "string",
                     sender_postal_code: "string"
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
