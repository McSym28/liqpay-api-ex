defmodule LiqPayAPI.InternetAcquiring.SubscriptionTest do
  use ExUnit.Case, async: true
  import Mox

  @httpoison OpenAPIClient.HTTPoisonMock
  @client OpenAPIClientMock

  setup :verify_on_exit!

  describe "edit/2" do
    test "[200] performs a request, encodes Edit.Request from request's body and decodes Edit.Response from response's body" do
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
                   "amount" => 3.0,
                   "amount_bonus" => 0.0,
                   "amount_credit" => 3.0,
                   "amount_debit" => 3.0,
                   "card_token" => "6E28D6039FD09FA3CE1DF27BCC2DE0E3254A3B27",
                   "commission_credit" => 0.57,
                   "commission_debit" => 0.0,
                   "create_date" => 1_705_655_382_848,
                   "currency" => "UAH",
                   "currency_credit" => "UAH",
                   "currency_debit" => "UAH",
                   "description" => "description",
                   "end_date" => 1_705_655_382_943,
                   "is_3ds" => true,
                   "liqpay_order_id" => "F9D77PKO1705655382844999",
                   "mpi_eci" => 7,
                   "order_id" => "order_id_263763737",
                   "payment_id" => 2_416_613_735,
                   "paytype" => "card",
                   "public_key" => "sandbox_i63492854596",
                   "receiver_commission" => 0.02,
                   "sender_bonus" => 0.0,
                   "sender_card_bank" => "Test",
                   "sender_card_country" => 804,
                   "sender_card_mask2" => "424242*42",
                   "sender_card_type" => "visa",
                   "sender_commission" => 0.0,
                   "sender_phone" => "380950000001",
                   "status" => "subscribed",
                   "transaction_id" => 2_416_613_735,
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
              %LiqPayAPI.InternetAcquiring.Subscription.Edit.Response{
                acq_id: 414_963,
                action: :subscribe,
                agent_commission: 0.0,
                amount: 3.0,
                amount_bonus: 0.0,
                amount_credit: 3.0,
                amount_debit: 3.0,
                card_token: "6E28D6039FD09FA3CE1DF27BCC2DE0E3254A3B27",
                commission_credit: 0.57,
                commission_debit: 0.0,
                create_date: ~U[2024-01-19 09:09:42.848Z],
                currency: "UAH",
                currency_credit: "UAH",
                currency_debit: "UAH",
                description: "description",
                end_date: ~U[2024-01-19 09:09:42.943Z],
                is_3ds: true,
                liqpay_order_id: "F9D77PKO1705655382844999",
                mpi_eci: 7,
                order_id: "order_id_263763737",
                payment_id: 2_416_613_735,
                paytype: :card,
                public_key: "sandbox_i63492854596",
                receiver_commission: 0.02,
                sender_bonus: 0.0,
                sender_card_bank: "Test",
                sender_card_country: 804,
                sender_card_mask2: "424242*42",
                sender_card_type: "visa",
                sender_commission: 0.0,
                sender_phone: "380950000001",
                status: :subscribed,
                transaction_id: 2_416_613_735,
                type: "buy",
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
                 private_key: "a4825234f4bae72a0be04eafe9e8e2bada209255",
                 base_url: "https://example.com"
               )
    end
  end

  describe "unsubscribe/2" do
    test "[200] performs a request, encodes Unsubscribe.Request from request's body and decodes Unsubscribe.Response from response's body" do
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
                   "amount" => 1.0,
                   "amount_bonus" => 0.0,
                   "amount_credit" => 38.31,
                   "amount_debit" => 38.31,
                   "card_token" => "6E28D6039FD09FA3CE1DF27BCC2DE0E3254A3B27",
                   "commission_credit" => 0.57,
                   "commission_debit" => 0.0,
                   "create_date" => 1_705_651_874_776,
                   "currency" => "USD",
                   "currency_credit" => "UAH",
                   "currency_debit" => "UAH",
                   "description" => "description",
                   "end_date" => 1_705_651_920_384,
                   "is_3ds" => true,
                   "liqpay_order_id" => "HGJI2AU61705651874774315",
                   "mpi_eci" => 7,
                   "order_id" => "order_id_76587576",
                   "payment_id" => 2_416_590_001,
                   "paytype" => "card",
                   "public_key" => "sandbox_i63492854596",
                   "receiver_commission" => 0.02,
                   "result" => "ok",
                   "sender_bonus" => 0.0,
                   "sender_card_bank" => "Test",
                   "sender_card_country" => 804,
                   "sender_card_mask2" => "424242*42",
                   "sender_card_type" => "visa",
                   "sender_commission" => 0.0,
                   "sender_phone" => "380950000001",
                   "status" => "unsubscribed",
                   "transaction_id" => 2_416_590_001,
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
              %LiqPayAPI.InternetAcquiring.Subscription.Unsubscribe.Response{
                acq_id: 414_963,
                action: :subscribe,
                agent_commission: 0.0,
                amount: 1.0,
                amount_bonus: 0.0,
                amount_credit: 38.31,
                amount_debit: 38.31,
                card_token: "6E28D6039FD09FA3CE1DF27BCC2DE0E3254A3B27",
                commission_credit: 0.57,
                commission_debit: 0.0,
                create_date: ~U[2024-01-19 08:11:14.776Z],
                currency: "USD",
                currency_credit: "UAH",
                currency_debit: "UAH",
                description: "description",
                end_date: ~U[2024-01-19 08:12:00.384Z],
                is_3ds: true,
                liqpay_order_id: "HGJI2AU61705651874774315",
                mpi_eci: 7,
                order_id: "order_id_76587576",
                payment_id: 2_416_590_001,
                paytype: :card,
                public_key: "sandbox_i63492854596",
                receiver_commission: 0.02,
                result: :ok,
                sender_bonus: 0.0,
                sender_card_bank: "Test",
                sender_card_country: 804,
                sender_card_mask2: "424242*42",
                sender_card_type: "visa",
                sender_commission: 0.0,
                sender_phone: "380950000001",
                status: :unsubscribed,
                transaction_id: 2_416_590_001,
                type: "buy",
                version: 3
              }} ==
               LiqPayAPI.InternetAcquiring.Subscription.unsubscribe(
                 %LiqPayAPI.InternetAcquiring.Subscription.Unsubscribe.Request{
                   action: :unsubscribe,
                   order_id: "order_id_1",
                   public_key: "string",
                   version: 3
                 },
                 private_key: "a4825234f4bae72a0be04eafe9e8e2bada209255",
                 base_url: "https://example.com"
               )
    end
  end

  describe "create/2" do
    test "[200] performs a request, encodes Create.Request from request's body and decodes Create.Response from response's body" do
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
                   "amount" => 0.1,
                   "amount_bonus" => 0.0,
                   "amount_credit" => 0.1,
                   "amount_debit" => 0.1,
                   "authcode_debit" => "944253",
                   "card_token" => "6E28D6039FD09FA3CE1DF27BCC2DE0E3254A3B27",
                   "commission_credit" => 0.0,
                   "commission_debit" => 0.0,
                   "create_date" => 1_715_266_869_912,
                   "currency" => "UAH",
                   "currency_credit" => "UAH",
                   "currency_debit" => "UAH",
                   "description" => "description text",
                   "end_date" => 1_715_266_990_142,
                   "is_3ds" => true,
                   "liqpay_order_id" => "1I9IGW951715266986946665",
                   "mpi_eci" => 7,
                   "order_id" => "order_id_263763yh737",
                   "payment_id" => 13_291_296,
                   "paytype" => "card",
                   "public_key" => "i16202663459",
                   "receiver_commission" => 0.0,
                   "result" => "ok",
                   "rrn_debit" => "000000454110",
                   "sender_bonus" => 0.0,
                   "sender_card_bank" => "pb",
                   "sender_card_country" => 804,
                   "sender_card_mask2" => "545708*24",
                   "sender_card_type" => "mc",
                   "sender_commission" => 0.0,
                   "sender_first_name" => "Тетяна",
                   "sender_last_name" => "Станько",
                   "sender_phone" => "380933454182",
                   "status" => "subscribed",
                   "transaction_id" => 13_291_296,
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
              %LiqPayAPI.InternetAcquiring.Subscription.Create.Response{
                acq_id: 414_963,
                action: :subscribe,
                agent_commission: 0.0,
                amount: 0.1,
                amount_bonus: 0.0,
                amount_credit: 0.1,
                amount_debit: 0.1,
                authcode_debit: "944253",
                card_token: "6E28D6039FD09FA3CE1DF27BCC2DE0E3254A3B27",
                commission_credit: 0.0,
                commission_debit: 0.0,
                create_date: ~U[2024-05-09 15:01:09.912Z],
                currency: "UAH",
                currency_credit: "UAH",
                currency_debit: "UAH",
                description: "description text",
                end_date: ~U[2024-05-09 15:03:10.142Z],
                is_3ds: true,
                liqpay_order_id: "1I9IGW951715266986946665",
                mpi_eci: 7,
                order_id: "order_id_263763yh737",
                payment_id: 13_291_296,
                paytype: :card,
                public_key: "i16202663459",
                receiver_commission: 0.0,
                result: :ok,
                rrn_debit: "000000454110",
                sender_bonus: 0.0,
                sender_card_bank: "pb",
                sender_card_country: 804,
                sender_card_mask2: "545708*24",
                sender_card_type: "mc",
                sender_commission: 0.0,
                sender_first_name: "Тетяна",
                sender_last_name: "Станько",
                sender_phone: "380933454182",
                status: :subscribed,
                transaction_id: 13_291_296,
                type: "buy",
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
                 private_key: "a4825234f4bae72a0be04eafe9e8e2bada209255",
                 base_url: "https://example.com"
               )
    end
  end
end
