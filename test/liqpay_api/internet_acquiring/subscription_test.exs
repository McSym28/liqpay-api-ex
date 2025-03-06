defmodule LiqPayAPI.InternetAcquiring.SubscriptionTest do
  use ExUnit.Case, async: true
  import Mox

  @httpoison OpenAPIClient.HTTPoisonMock
  @client OpenAPIClientMock

  setup :verify_on_exit!

  describe "edit/2" do
    test "[200] performs a request, encodes Edit.Request from request's body and decodes Edit.Response from response's body" do
      expect(@client, :operation, &OpenAPIClient.operation/2)

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
        assert LiqPayAPI.Client.Signature.check?(data, nil, signature)
        assert {:ok, body} = Base.decode64(data)
        headers = List.keystore(headers, "content-type", 0, {"content-type", "application/json"})
        assert {:ok, "application/json"} == OpenAPIClient.Utils.get_content_type(headers)

        assert {:ok,
                %{
                  "action" => "subscribe_update",
                  "amount" => 5.0,
                  "currency" => "USD",
                  "description" => "description text",
                  "order_id" => "order_id_1",
                  "public_key" => "string",
                  "version" => 3
                }} == Jason.decode(body)

        assert {:ok, body_encoded} =
                 Jason.encode(%{
                   "acq_id" => 414_963,
                   "action" => "subscribe",
                   "agent_commission" => 0.0,
                   "amount" => 0.02,
                   "amount_bonus" => 0.0,
                   "amount_credit" => 0.02,
                   "amount_debit" => 0.02,
                   "card_token" => "2DFBFE846B734166230DE81E971E948D6F260",
                   "commission_credit" => 0.0,
                   "commission_debit" => 0.0,
                   "create_date" => 1_501_680_854_599,
                   "currency" => "UAH",
                   "currency_credit" => "UAH",
                   "currency_debit" => "UAH",
                   "description" => "description text",
                   "end_date" => 1_501_680_854_599,
                   "is_3ds" => true,
                   "liqpay_order_id" => "NYMK3AE61501685438251925",
                   "mpi_eci" => 7,
                   "order_id" => "98R1U1OV1485849059893399",
                   "payment_id" => 165_156,
                   "paytype" => "token",
                   "public_key" => "i000000000",
                   "receiver_commission" => 0.0,
                   "sender_bonus" => 0.0,
                   "sender_card_bank" => "pb",
                   "sender_card_country" => 804,
                   "sender_card_mask2" => "414961*99",
                   "sender_card_type" => "visa",
                   "sender_commission" => 0.0,
                   "sender_phone" => "380950000001",
                   "status" => "subscribed",
                   "transaction_id" => 165_156,
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
              %LiqPayAPI.InternetAcquiring.Subscription.Edit.Response{
                acq_id: 414_963,
                action: :subscribe,
                agent_commission: 0.0,
                amount: 0.02,
                amount_bonus: 0.0,
                amount_credit: 0.02,
                amount_debit: 0.02,
                card_token: "2DFBFE846B734166230DE81E971E948D6F260",
                commission_credit: 0.0,
                commission_debit: 0.0,
                create_date: ~U[2017-08-02 13:34:14.599Z],
                currency: "UAH",
                currency_credit: "UAH",
                currency_debit: "UAH",
                description: "description text",
                end_date: ~U[2017-08-02 13:34:14.599Z],
                is_3ds: true,
                liqpay_order_id: "NYMK3AE61501685438251925",
                mpi_eci: 7,
                order_id: "98R1U1OV1485849059893399",
                payment_id: 165_156,
                paytype: "token",
                public_key: "i000000000",
                receiver_commission: 0.0,
                sender_bonus: 0.0,
                sender_card_bank: "pb",
                sender_card_country: "804",
                sender_card_mask2: "414961*99",
                sender_card_type: "visa",
                sender_commission: 0.0,
                sender_phone: "380950000001",
                status: :subscribed,
                transaction_id: 165_156,
                version: 3
              }} ==
               LiqPayAPI.InternetAcquiring.Subscription.edit(
                 %LiqPayAPI.InternetAcquiring.Subscription.Edit.Request{
                   action: :subscribe_update,
                   amount: 5.0,
                   currency: :usd,
                   description: "description text",
                   order_id: "order_id_1",
                   public_key: "string",
                   version: 3
                 },
                 base_url: "https://example.com"
               )
    end
  end

  describe "unsubscribe/2" do
    test "[200] performs a request, encodes Unsubscribe.Request from request's body and decodes Unsubscribe.Response from response's body" do
      expect(@client, :operation, &OpenAPIClient.operation/2)

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
        assert LiqPayAPI.Client.Signature.check?(data, nil, signature)
        assert {:ok, body} = Base.decode64(data)
        headers = List.keystore(headers, "content-type", 0, {"content-type", "application/json"})
        assert {:ok, "application/json"} == OpenAPIClient.Utils.get_content_type(headers)

        assert {:ok,
                %{
                  "action" => "unsubscribe",
                  "order_id" => "order_id_1",
                  "public_key" => "string",
                  "version" => 3
                }} == Jason.decode(body)

        assert {:ok, body_encoded} =
                 Jason.encode(%{
                   "acq_id" => 414_963,
                   "action" => "subscribe",
                   "agent_commission" => 0.0,
                   "amount" => 0.02,
                   "amount_bonus" => 0.0,
                   "amount_credit" => 0.02,
                   "amount_debit" => 0.02,
                   "card_token" => "2DFBFE626B730006230DE81E971E6588D6F260",
                   "commission_credit" => 0.0,
                   "commission_debit" => 0.0,
                   "create_date" => 1_501_680_852_705,
                   "currency" => "UAH",
                   "currency_credit" => "UAH",
                   "currency_debit" => "UAH",
                   "description" => "description text",
                   "end_date" => 1_501_681_041_689,
                   "is_3ds" => true,
                   "liqpay_order_id" => "NYMK3AE61501685448251925",
                   "mpi_eci" => 7,
                   "order_id" => "98R1U1OV1400849059893399",
                   "payment_id" => 165_156,
                   "paytype" => "token",
                   "public_key" => "i000000000",
                   "receiver_commission" => 0.0,
                   "sender_bonus" => 0.0,
                   "sender_card_bank" => "pb",
                   "sender_card_country" => 804,
                   "sender_card_mask2" => "414961*99",
                   "sender_card_type" => "visa",
                   "sender_commission" => 0.0,
                   "sender_phone" => "380950000001",
                   "status" => "unsubscribed",
                   "transaction_id" => 165_156,
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
              %LiqPayAPI.InternetAcquiring.Subscription.Unsubscribe.Response{
                acq_id: 414_963,
                action: :subscribe,
                agent_commission: 0.0,
                amount: 0.02,
                amount_bonus: 0.0,
                amount_credit: 0.02,
                amount_debit: 0.02,
                card_token: "2DFBFE626B730006230DE81E971E6588D6F260",
                commission_credit: 0.0,
                commission_debit: 0.0,
                create_date: ~U[2017-08-02 13:34:12.705Z],
                currency: "UAH",
                currency_credit: "UAH",
                currency_debit: "UAH",
                description: "description text",
                end_date: ~U[2017-08-02 13:37:21.689Z],
                is_3ds: true,
                liqpay_order_id: "NYMK3AE61501685448251925",
                mpi_eci: 7,
                order_id: "98R1U1OV1400849059893399",
                payment_id: 165_156,
                paytype: "token",
                public_key: "i000000000",
                receiver_commission: 0.0,
                sender_bonus: 0.0,
                sender_card_bank: "pb",
                sender_card_country: "804",
                sender_card_mask2: "414961*99",
                sender_card_type: "visa",
                sender_commission: 0.0,
                sender_phone: "380950000001",
                status: :unsubscribed,
                transaction_id: 165_156,
                version: 3
              }} ==
               LiqPayAPI.InternetAcquiring.Subscription.unsubscribe(
                 %LiqPayAPI.InternetAcquiring.Subscription.Unsubscribe.Request{
                   action: :unsubscribe,
                   order_id: "order_id_1",
                   public_key: "string",
                   version: 3
                 },
                 base_url: "https://example.com"
               )
    end
  end

  describe "create/2" do
    test "[200] performs a request, encodes Create.Request from request's body and decodes Create.Response from response's body" do
      expect(@client, :operation, &OpenAPIClient.operation/2)

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
        assert LiqPayAPI.Client.Signature.check?(data, nil, signature)
        assert {:ok, body} = Base.decode64(data)
        headers = List.keystore(headers, "content-type", 0, {"content-type", "application/json"})
        assert {:ok, "application/json"} == OpenAPIClient.Utils.get_content_type(headers)

        assert {:ok,
                %{
                  "action" => "subscribe",
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
                  "phone" => "+380950000001",
                  "prepare" => "1",
                  "product_category" => "string",
                  "product_description" => "string",
                  "product_name" => "string",
                  "product_url" => "http://example.com",
                  "public_key" => "string",
                  "recurring" => true,
                  "recurringbytoken" => "1",
                  "sender_address" => "string",
                  "sender_city" => "string",
                  "sender_country_code" => "string",
                  "sender_first_name" => "string",
                  "sender_last_name" => "string",
                  "sender_postal_code" => "string",
                  "server_url" => "http://example.com",
                  "subscribe" => "1",
                  "subscribe_date_start" => "2015-03-31 00:00:00",
                  "subscribe_periodicity" => "month",
                  "version" => 3
                }} == Jason.decode(body)

        assert {:ok, body_encoded} =
                 Jason.encode(%{
                   "acq_id" => 414_963,
                   "action" => "subscribe",
                   "agent_commission" => 0.0,
                   "amount" => 0.02,
                   "amount_bonus" => 0.0,
                   "amount_credit" => 0.02,
                   "amount_debit" => 0.02,
                   "card_token" => "2DFBFE846B734166230DE81E971E948D6F260",
                   "commission_credit" => 0.0,
                   "commission_debit" => 0.0,
                   "create_date" => 1_501_680_854_599,
                   "currency" => "UAH",
                   "currency_credit" => "UAH",
                   "currency_debit" => "UAH",
                   "description" => "description text",
                   "end_date" => 1_501_680_854_599,
                   "is_3ds" => true,
                   "liqpay_order_id" => "NYMK3AE61501685438251925",
                   "mpi_eci" => 7,
                   "order_id" => "98R1U1OV1485849059893399",
                   "payment_id" => 165_156,
                   "paytype" => "token",
                   "public_key" => "i000000000",
                   "receiver_commission" => 0.0,
                   "sender_bonus" => 0.0,
                   "sender_card_bank" => "pb",
                   "sender_card_country" => 804,
                   "sender_card_mask2" => "414961*99",
                   "sender_card_type" => "visa",
                   "sender_commission" => 0.0,
                   "sender_phone" => "380950000001",
                   "status" => "subscribed",
                   "transaction_id" => 165_156,
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
              %LiqPayAPI.InternetAcquiring.Subscription.Create.Response{
                acq_id: 414_963,
                action: :subscribe,
                agent_commission: 0.0,
                amount: 0.02,
                amount_bonus: 0.0,
                amount_credit: 0.02,
                amount_debit: 0.02,
                card_token: "2DFBFE846B734166230DE81E971E948D6F260",
                commission_credit: 0.0,
                commission_debit: 0.0,
                create_date: ~U[2017-08-02 13:34:14.599Z],
                currency: "UAH",
                currency_credit: "UAH",
                currency_debit: "UAH",
                description: "description text",
                end_date: ~U[2017-08-02 13:34:14.599Z],
                is_3ds: true,
                liqpay_order_id: "NYMK3AE61501685438251925",
                mpi_eci: 7,
                order_id: "98R1U1OV1485849059893399",
                payment_id: 165_156,
                paytype: "token",
                public_key: "i000000000",
                receiver_commission: 0.0,
                sender_bonus: 0.0,
                sender_card_bank: "pb",
                sender_card_country: "804",
                sender_card_mask2: "414961*99",
                sender_card_type: "visa",
                sender_commission: 0.0,
                sender_phone: "380950000001",
                status: :subscribed,
                transaction_id: 165_156,
                version: 3
              }} ==
               LiqPayAPI.InternetAcquiring.Subscription.create(
                 %LiqPayAPI.InternetAcquiring.Subscription.Create.Request{
                   action: :subscribe,
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
                     %LiqPayAPI.InternetAcquiring.Subscription.Create.Request.RegularPayment{
                       subscribe: :"1",
                       subscribe_date_start: ~U[2015-03-31 00:00:00Z],
                       subscribe_periodicity: :month
                     },
                   sender: %LiqPayAPI.InternetAcquiring.Subscription.Create.Request.Sender{
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
                 base_url: "https://example.com"
               )
    end
  end
end
