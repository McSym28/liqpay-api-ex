defmodule LiqPayAPI.InternetAcquiringTest do
  use ExUnit.Case, async: true
  import Mox

  @httpoison OpenAPIClient.HTTPoisonMock
  @client OpenAPIClient.ClientMock

  setup :verify_on_exit!

  describe "cash/2" do
    test "[200] performs a request, encodes Cash.Request from request's body and encodes Cash.Response from response's body" do
      expect(@client, :perform, fn operation, pipeline ->
        params = OpenAPIClient.Client.Operation.get_private(operation, :__params__)

        assert {_, "a4825234f4bae72a0be04eafe9e8e2bada209255"} =
                 List.keyfind(params, :private_key, 0)

        OpenAPIClient.Client.perform(operation, pipeline)
      end)

      expect(@httpoison, :request, fn :post,
                                      "https://example.com/api/request",
                                      body,
                                      headers,
                                      _ ->
        assert {:ok, "application/x-www-form-urlencoded"} ==
                 (with {_, content_type_request} <- List.keyfind(headers, "content-type", 0),
                       {:ok, {media_type, media_subtype, _parameters}} =
                         OpenAPIClient.Client.Operation.parse_content_type_header(
                           content_type_request
                         ) do
                    {:ok, "#{media_type}/#{media_subtype}"}
                  end)

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

        assert {:ok, "application/json"} ==
                 (with {_, content_type_request} <- List.keyfind(headers, "content-type", 0),
                       {:ok, {media_type, media_subtype, _parameters}} =
                         OpenAPIClient.Client.Operation.parse_content_type_header(
                           content_type_request
                         ) do
                    {:ok, "#{media_type}/#{media_subtype}"}
                  end)

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
                      "amount" => 1,
                      "commission_payer" => "sender",
                      "description" => "string",
                      "public_key" => "i000000001",
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
                   "amount" => 0.1,
                   "amount_bonus" => 0.0,
                   "amount_credit" => 0.1,
                   "amount_debit" => 0.1,
                   "commission_credit" => 0.0,
                   "commission_debit" => 0.0,
                   "currency" => "UAH",
                   "currency_credit" => "UAH",
                   "currency_debit" => "UAH",
                   "description" => "description",
                   "end_date" => 1_706_750_625_987,
                   "ip" => "string",
                   "is_3ds" => true,
                   "liqpay_order_id" => "NYMK3AE61501685438251925",
                   "mpi_eci" => 7,
                   "order_id" => "98R1U1OV1485849059893399",
                   "payment_id" => 165_168,
                   "public_key" => "i000000000",
                   "receiver_commission" => 0.0,
                   "sender_bonus" => 0.0,
                   "sender_commission" => 0.0,
                   "sender_first_name" => "first name",
                   "sender_last_name" => "last name",
                   "sender_phone" => "380950000001",
                   "status" => "cash_wait",
                   "transaction_id" => 165_168,
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
                amount: 0.1,
                amount_bonus: 0.0,
                amount_credit: 0.1,
                amount_debit: 0.1,
                commission_credit: 0.0,
                commission_debit: 0.0,
                currency: "UAH",
                currency_credit: "UAH",
                currency_debit: "UAH",
                description: "description",
                end_date: ~U[2024-02-01 01:23:45.987Z],
                ip: "string",
                is_3ds: true,
                liqpay_order_id: "NYMK3AE61501685438251925",
                mpi_eci: 7,
                order_id: "98R1U1OV1485849059893399",
                payment_id: 165_168,
                public_key: "i000000000",
                receiver_commission: 0.0,
                sender_bonus: 0.0,
                sender_commission: 0.0,
                sender_first_name: "first name",
                sender_last_name: "last name",
                sender_phone: "380950000001",
                status: :cash_wait,
                transaction_id: 165_168,
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
                       amount: 1,
                       commission_payer: :sender,
                       description: "string",
                       public_key: "i000000001",
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

  describe "qr/2" do
    test "[200] performs a request, encodes QR.Request from request's body and encodes QR.Response from response's body" do
      expect(@client, :perform, fn operation, pipeline ->
        params = OpenAPIClient.Client.Operation.get_private(operation, :__params__)

        assert {_, "a4825234f4bae72a0be04eafe9e8e2bada209255"} =
                 List.keyfind(params, :private_key, 0)

        OpenAPIClient.Client.perform(operation, pipeline)
      end)

      expect(@httpoison, :request, fn :post,
                                      "https://example.com/api/request",
                                      body,
                                      headers,
                                      _ ->
        assert {:ok, "application/x-www-form-urlencoded"} ==
                 (with {_, content_type_request} <- List.keyfind(headers, "content-type", 0),
                       {:ok, {media_type, media_subtype, _parameters}} =
                         OpenAPIClient.Client.Operation.parse_content_type_header(
                           content_type_request
                         ) do
                    {:ok, "#{media_type}/#{media_subtype}"}
                  end)

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

        assert {:ok, "application/json"} ==
                 (with {_, content_type_request} <- List.keyfind(headers, "content-type", 0),
                       {:ok, {media_type, media_subtype, _parameters}} =
                         OpenAPIClient.Client.Operation.parse_content_type_header(
                           content_type_request
                         ) do
                    {:ok, "#{media_type}/#{media_subtype}"}
                  end)

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
                      "amount" => 1,
                      "commission_payer" => "sender",
                      "description" => "string",
                      "public_key" => "i000000001",
                      "server_url" => "https://server1/callback"
                    }
                  ],
                  "version" => 3
                }} == Jason.decode(body)

        assert {:ok, body_encoded} =
                 Jason.encode(%{
                   "acq_id" => 414_963,
                   "action" => "payqr",
                   "agent_commission" => 0.0,
                   "amount" => 0.02,
                   "amount_bonus" => 0.0,
                   "amount_credit" => 0.02,
                   "amount_debit" => 0.02,
                   "commission_credit" => 0.0,
                   "commission_debit" => 0.0,
                   "currency" => "UAH",
                   "currency_credit" => "UAH",
                   "currency_debit" => "UAH",
                   "description" => "description",
                   "end_date" => 1_706_750_625_987,
                   "ip" => "string",
                   "is_3ds" => true,
                   "liqpay_order_id" => "NYMK3AE61501685438251925",
                   "mpi_eci" => 7,
                   "order_id" => "98R1U1OV1485849059893399",
                   "payment_id" => 165_164,
                   "public_key" => "i000000000",
                   "qr_code" => "lp_qr:pay:nC4O1DG2nUsMI44Zh80utdjYvw",
                   "receiver_commission" => 0.0,
                   "sender_bonus" => 0.0,
                   "sender_commission" => 0.0,
                   "status" => "wait_qr",
                   "transaction_id" => 165_164,
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
              %LiqPayAPI.InternetAcquiring.QR.Response{
                acq_id: 414_963,
                action: "payqr",
                agent_commission: 0.0,
                amount: 0.02,
                amount_bonus: 0.0,
                amount_credit: 0.02,
                amount_debit: 0.02,
                commission_credit: 0.0,
                commission_debit: 0.0,
                currency: "UAH",
                currency_credit: "UAH",
                currency_debit: "UAH",
                description: "description",
                end_date: ~U[2024-02-01 01:23:45.987Z],
                ip: "string",
                is_3ds: true,
                liqpay_order_id: "NYMK3AE61501685438251925",
                mpi_eci: 7,
                order_id: "98R1U1OV1485849059893399",
                payment_id: 165_164,
                public_key: "i000000000",
                qr_code: "lp_qr:pay:nC4O1DG2nUsMI44Zh80utdjYvw",
                receiver_commission: 0.0,
                sender_bonus: 0.0,
                sender_commission: 0.0,
                status: :wait_qr,
                transaction_id: 165_164,
                type: "buy",
                version: 3
              }} ==
               LiqPayAPI.InternetAcquiring.qr(
                 %LiqPayAPI.InternetAcquiring.QR.Request{
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
                     %LiqPayAPI.InternetAcquiring.QR.Request.SplitRules{
                       amount: 1,
                       commission_payer: :sender,
                       description: "string",
                       public_key: "i000000001",
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
    test "[200] performs a request, encodes CardPayment.Request from request's body and encodes CardPayment.Response from response's body" do
      expect(@client, :perform, fn operation, pipeline ->
        params = OpenAPIClient.Client.Operation.get_private(operation, :__params__)

        assert {_, "a4825234f4bae72a0be04eafe9e8e2bada209255"} =
                 List.keyfind(params, :private_key, 0)

        OpenAPIClient.Client.perform(operation, pipeline)
      end)

      expect(@httpoison, :request, fn :post,
                                      "https://example.com/api/request",
                                      body,
                                      headers,
                                      _ ->
        assert {:ok, "application/x-www-form-urlencoded"} ==
                 (with {_, content_type_request} <- List.keyfind(headers, "content-type", 0),
                       {:ok, {media_type, media_subtype, _parameters}} =
                         OpenAPIClient.Client.Operation.parse_content_type_header(
                           content_type_request
                         ) do
                    {:ok, "#{media_type}/#{media_subtype}"}
                  end)

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

        assert {:ok, "application/json"} ==
                 (with {_, content_type_request} <- List.keyfind(headers, "content-type", 0),
                       {:ok, {media_type, media_subtype, _parameters}} =
                         OpenAPIClient.Client.Operation.parse_content_type_header(
                           content_type_request
                         ) do
                    {:ok, "#{media_type}/#{media_subtype}"}
                  end)

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
                      "amount" => 1,
                      "commission_payer" => "sender",
                      "description" => "string",
                      "public_key" => "i000000001",
                      "server_url" => "https://server1/callback"
                    }
                  ],
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
                   "liqpay_order_id" => "NYMK3AE61501685438251925",
                   "mpi_cres" => "string",
                   "mpi_eci" => 7,
                   "order_id" => "98R1U1OV1485849059893399",
                   "payment_id" => 165_172,
                   "paytype" => "card",
                   "public_key" => "i000000000",
                   "receiver_commission" => 0.0,
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
                liqpay_order_id: "NYMK3AE61501685438251925",
                mpi_cres: "string",
                mpi_eci: 7,
                order_id: "98R1U1OV1485849059893399",
                payment_id: 165_172,
                paytype: :card,
                public_key: "i000000000",
                receiver_commission: 0.0,
                rrn_credit: "000663747003",
                rrn_debit: "000663747000",
                sender_bonus: 0.0,
                sender_card_bank: "pb",
                sender_card_country: "804",
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
                       amount: 1,
                       commission_payer: :sender,
                       description: "string",
                       public_key: "i000000001",
                       server_url: "https://server1/callback"
                     }
                   ],
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
    test "[200] performs a request, encodes Token.Request from request's body and encodes Token.Response from response's body" do
      expect(@client, :perform, fn operation, pipeline ->
        params = OpenAPIClient.Client.Operation.get_private(operation, :__params__)

        assert {_, "a4825234f4bae72a0be04eafe9e8e2bada209255"} =
                 List.keyfind(params, :private_key, 0)

        OpenAPIClient.Client.perform(operation, pipeline)
      end)

      expect(@httpoison, :request, fn :post,
                                      "https://example.com/api/request",
                                      body,
                                      headers,
                                      _ ->
        assert {:ok, "application/x-www-form-urlencoded"} ==
                 (with {_, content_type_request} <- List.keyfind(headers, "content-type", 0),
                       {:ok, {media_type, media_subtype, _parameters}} =
                         OpenAPIClient.Client.Operation.parse_content_type_header(
                           content_type_request
                         ) do
                    {:ok, "#{media_type}/#{media_subtype}"}
                  end)

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

        assert {:ok, "application/json"} ==
                 (with {_, content_type_request} <- List.keyfind(headers, "content-type", 0),
                       {:ok, {media_type, media_subtype, _parameters}} =
                         OpenAPIClient.Client.Operation.parse_content_type_header(
                           content_type_request
                         ) do
                    {:ok, "#{media_type}/#{media_subtype}"}
                  end)

        assert {:ok,
                %{
                  "action" => "paytoken",
                  "amount" => 5.0,
                  "card_token" => "B5BВB0D00B88B00ED00A00D0D",
                  "currency" => "USD",
                  "customer" => "string",
                  "dae" => "string",
                  "description" => "description text",
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
                  "sender_address" => "string",
                  "sender_city" => "string",
                  "sender_country_code" => "string",
                  "sender_first_name" => "string",
                  "sender_last_name" => "string",
                  "sender_postal_code" => "string",
                  "server_url" => "http://example.com",
                  "split_rules" => [
                    %{
                      "amount" => 1,
                      "commission_payer" => "sender",
                      "description" => "string",
                      "public_key" => "i000000001",
                      "server_url" => "https://server1/callback"
                    }
                  ],
                  "version" => 3
                }} == Jason.decode(body)

        assert {:ok, body_encoded} =
                 Jason.encode(%{
                   "acq_id" => 414_963,
                   "action" => "pay",
                   "agent_commission" => 0.0,
                   "amount" => 0.02,
                   "amount_bonus" => 0.0,
                   "amount_credit" => 0.02,
                   "amount_debit" => 0.02,
                   "authcode_credit" => "538974",
                   "authcode_debit" => "366297",
                   "bonus_procent" => 7.0,
                   "bonus_type" => "bonusplus",
                   "commission_credit" => 0.0,
                   "commission_debit" => 0.0,
                   "create_date" => 1_501_684_842_777,
                   "currency" => "UAH",
                   "currency_credit" => "UAH",
                   "currency_debit" => "UAH",
                   "description" => "description text",
                   "end_date" => 1_501_684_842_777,
                   "is_3ds" => true,
                   "liqpay_order_id" => "NYMK3AE61501685438251925",
                   "mpi_eci" => 7,
                   "order_id" => "98R1U1OV1485849059893399",
                   "payment_id" => 165_167,
                   "paytype" => "card",
                   "public_key" => "i000000000",
                   "receiver_commission" => 0.0,
                   "rrn_credit" => "000663740472",
                   "rrn_debit" => "000663740464",
                   "sender_bonus" => 0.0,
                   "sender_card_bank" => "pb",
                   "sender_card_country" => 804,
                   "sender_card_mask2" => "414961*99",
                   "sender_card_type" => "visa",
                   "sender_commission" => 0.0,
                   "sender_phone" => "380950000001",
                   "status" => "success",
                   "transaction_id" => 165_167,
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
                amount: 0.02,
                amount_bonus: 0.0,
                amount_credit: 0.02,
                amount_debit: 0.02,
                authcode_credit: "538974",
                authcode_debit: "366297",
                bonus_procent: 7.0,
                bonus_type: :bonusplus,
                commission_credit: 0.0,
                commission_debit: 0.0,
                create_date: ~U[2017-08-02 14:40:42.777Z],
                currency: "UAH",
                currency_credit: "UAH",
                currency_debit: "UAH",
                description: "description text",
                end_date: ~U[2017-08-02 14:40:42.777Z],
                is_3ds: true,
                liqpay_order_id: "NYMK3AE61501685438251925",
                mpi_eci: 7,
                order_id: "98R1U1OV1485849059893399",
                payment_id: 165_167,
                paytype: :card,
                public_key: "i000000000",
                receiver_commission: 0.0,
                rrn_credit: "000663740472",
                rrn_debit: "000663740464",
                sender_bonus: 0.0,
                sender_card_bank: "pb",
                sender_card_country: "804",
                sender_card_mask2: "414961*99",
                sender_card_type: "visa",
                sender_commission: 0.0,
                sender_phone: "380950000001",
                status: :success,
                transaction_id: 165_167,
                type: "buy",
                version: 3
              }} ==
               LiqPayAPI.InternetAcquiring.token(
                 %LiqPayAPI.InternetAcquiring.Token.Request{
                   action: :paytoken,
                   amount: 5.0,
                   card_token: "B5BВB0D00B88B00ED00A00D0D",
                   currency: :usd,
                   customer: "string",
                   dae: "string",
                   description: "description text",
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
                       amount: 1,
                       commission_payer: :sender,
                       description: "string",
                       public_key: "i000000001",
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

  describe "refund/2" do
    test "[200] performs a request, encodes Refund.Request from request's body and encodes Refund.Response from response's body" do
      expect(@client, :perform, fn operation, pipeline ->
        params = OpenAPIClient.Client.Operation.get_private(operation, :__params__)

        assert {_, "a4825234f4bae72a0be04eafe9e8e2bada209255"} =
                 List.keyfind(params, :private_key, 0)

        OpenAPIClient.Client.perform(operation, pipeline)
      end)

      expect(@httpoison, :request, fn :post,
                                      "https://example.com/api/request",
                                      body,
                                      headers,
                                      _ ->
        assert {:ok, "application/x-www-form-urlencoded"} ==
                 (with {_, content_type_request} <- List.keyfind(headers, "content-type", 0),
                       {:ok, {media_type, media_subtype, _parameters}} =
                         OpenAPIClient.Client.Operation.parse_content_type_header(
                           content_type_request
                         ) do
                    {:ok, "#{media_type}/#{media_subtype}"}
                  end)

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

        assert {:ok, "application/json"} ==
                 (with {_, content_type_request} <- List.keyfind(headers, "content-type", 0),
                       {:ok, {media_type, media_subtype, _parameters}} =
                         OpenAPIClient.Client.Operation.parse_content_type_header(
                           content_type_request
                         ) do
                    {:ok, "#{media_type}/#{media_subtype}"}
                  end)

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
                   "action" => "refund",
                   "payment_id" => 165_173,
                   "status" => "reversed"
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
                action: "refund",
                payment_id: 165_173,
                status: :reversed
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
    test "[200] performs a request, encodes DCC.Request from request's body and encodes DCC.Response from response's body" do
      expect(@client, :perform, fn operation, pipeline ->
        params = OpenAPIClient.Client.Operation.get_private(operation, :__params__)

        assert {_, "a4825234f4bae72a0be04eafe9e8e2bada209255"} =
                 List.keyfind(params, :private_key, 0)

        OpenAPIClient.Client.perform(operation, pipeline)
      end)

      expect(@httpoison, :request, fn :post,
                                      "https://example.com/api/request",
                                      body,
                                      headers,
                                      _ ->
        assert {:ok, "application/x-www-form-urlencoded"} ==
                 (with {_, content_type_request} <- List.keyfind(headers, "content-type", 0),
                       {:ok, {media_type, media_subtype, _parameters}} =
                         OpenAPIClient.Client.Operation.parse_content_type_header(
                           content_type_request
                         ) do
                    {:ok, "#{media_type}/#{media_subtype}"}
                  end)

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

        assert {:ok, "application/json"} ==
                 (with {_, content_type_request} <- List.keyfind(headers, "content-type", 0),
                       {:ok, {media_type, media_subtype, _parameters}} =
                         OpenAPIClient.Client.Operation.parse_content_type_header(
                           content_type_request
                         ) do
                    {:ok, "#{media_type}/#{media_subtype}"}
                  end)

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
                   "liqpay_order_id" => "NYMK3AE61501685438251925",
                   "mpi_cres" => "string",
                   "mpi_eci" => 7,
                   "order_id" => "98R1U1OV1485849059893399",
                   "payment_id" => 165_172,
                   "paytype" => "card",
                   "public_key" => "i000000000",
                   "receiver_commission" => 0.0,
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
              %LiqPayAPI.InternetAcquiring.DCC.Response{
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
                create_date: ~U[2017-08-02 14:50:46.633Z],
                currency: "UAH",
                currency_credit: "UAH",
                currency_debit: "UAH",
                dcc_allowed: [
                  %LiqPayAPI.InternetAcquiring.DCC.Response.DCCAllowed{
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
                liqpay_order_id: "NYMK3AE61501685438251925",
                mpi_cres: "string",
                mpi_eci: 7,
                order_id: "98R1U1OV1485849059893399",
                payment_id: 165_172,
                paytype: :card,
                public_key: "i000000000",
                receiver_commission: 0.0,
                rrn_credit: "000663747003",
                rrn_debit: "000663747000",
                sender_bonus: 0.0,
                sender_card_bank: "pb",
                sender_card_country: "804",
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
