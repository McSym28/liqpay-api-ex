defmodule LiqPayAPI.InternetAcquiring.TwoStepTest do
  use ExUnit.Case, async: true
  import Mox

  @httpoison OpenAPIClient.HTTPoisonMock
  @client OpenAPIClientMock

  setup :verify_on_exit!

  describe "complete/2" do
    test "[200] performs a request, encodes Complete.Request from request's body and decodes Complete.Response from response's body" do
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
                  "action" => "hold_completion",
                  "amount" => 5.0,
                  "order_id" => "order_id_1",
                  "public_key" => "string",
                  "rro_info" => %{
                    "delivery_emails" => ["string"],
                    "items" => [%{"amount" => 2, "cost" => 404, "id" => 123_456, "price" => 202}]
                  },
                  "version" => 3
                }} == Jason.decode(body)

        assert {:ok, body_encoded} =
                 Jason.encode(%{
                   "acq_id" => 414_963,
                   "action" => "hold",
                   "agent_commission" => 0.0,
                   "amount" => 0.02,
                   "amount_bonus" => 0.0,
                   "amount_credit" => 0.02,
                   "amount_debit" => 0.02,
                   "authcode_credit" => "002970",
                   "authcode_debit" => "751279",
                   "card_token" => "2DFBFE626B734161130DE81E971E948D6F260",
                   "commission_credit" => 0.0,
                   "commission_debit" => 0.0,
                   "completion_date" => 1_501_681_656_973,
                   "create_date" => 1_501_681_569_734,
                   "currency" => "UAH",
                   "currency_credit" => "UAH",
                   "currency_debit" => "UAH",
                   "description" => "test",
                   "end_date" => 1_501_681_657_732,
                   "is_3ds" => true,
                   "liqpay_order_id" => "NYMK3AE61501685438251925",
                   "mpi_eci" => 7,
                   "order_id" => "98R1U1OV1485849059893399",
                   "payment_id" => 165_160,
                   "paytype" => "card",
                   "public_key" => "i000000000",
                   "receiver_commission" => 0.0,
                   "rrn_credit" => "000663205525",
                   "rrn_debit" => "000663204729",
                   "sender_bonus" => 0.0,
                   "sender_card_bank" => "pb",
                   "sender_card_country" => 804,
                   "sender_card_mask2" => "414961*99",
                   "sender_card_type" => "visa",
                   "sender_commission" => 0.0,
                   "sender_first_name" => "first name",
                   "sender_last_name" => "last name",
                   "sender_phone" => "380950000001",
                   "status" => "success",
                   "transaction_id" => 165_160,
                   "type" => "hold",
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
              %LiqPayAPI.InternetAcquiring.TwoStep.Complete.Response{
                acq_id: 414_963,
                action: :hold,
                agent_commission: 0.0,
                amount: 0.02,
                amount_bonus: 0.0,
                amount_credit: 0.02,
                amount_debit: 0.02,
                authcode_credit: "002970",
                authcode_debit: "751279",
                card_token: "2DFBFE626B734161130DE81E971E948D6F260",
                commission_credit: 0.0,
                commission_debit: 0.0,
                completion_date: ~U[2017-08-02 13:47:36.973Z],
                create_date: ~U[2017-08-02 13:46:09.734Z],
                currency: "UAH",
                currency_credit: "UAH",
                currency_debit: "UAH",
                description: "test",
                end_date: ~U[2017-08-02 13:47:37.732Z],
                is_3ds: true,
                liqpay_order_id: "NYMK3AE61501685438251925",
                mpi_eci: 7,
                order_id: "98R1U1OV1485849059893399",
                payment_id: 165_160,
                paytype: :card,
                public_key: "i000000000",
                receiver_commission: 0.0,
                rrn_credit: "000663205525",
                rrn_debit: "000663204729",
                sender_bonus: 0.0,
                sender_card_bank: "pb",
                sender_card_country: "804",
                sender_card_mask2: "414961*99",
                sender_card_type: "visa",
                sender_commission: 0.0,
                sender_first_name: "first name",
                sender_last_name: "last name",
                sender_phone: "380950000001",
                status: :success,
                transaction_id: 165_160,
                type: "hold",
                version: 3
              }} ==
               LiqPayAPI.InternetAcquiring.TwoStep.complete(
                 %LiqPayAPI.InternetAcquiring.TwoStep.Complete.Request{
                   action: :hold_completion,
                   amount: 5.0,
                   order_id: "order_id_1",
                   public_key: "string",
                   rro_info: %LiqPayAPI.InternetAcquiring.TwoStep.Complete.Request.RROInfo{
                     delivery_emails: ["string"],
                     items: [
                       %LiqPayAPI.InternetAcquiring.TwoStep.Complete.Request.RROInfo.Items{
                         amount: 2,
                         cost: 404,
                         id: 123_456,
                         price: 202
                       }
                     ]
                   },
                   version: 3
                 },
                 private_key: "a4825234f4bae72a0be04eafe9e8e2bada209255",
                 base_url: "https://example.com"
               )
    end
  end

  describe "block/2" do
    test "[200] performs a request, encodes Block.Request from request's body and decodes Block.Response from response's body" do
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
                  "action" => "hold",
                  "amount" => 5.0,
                  "apay_token" => "string",
                  "card" => "4731195301524634",
                  "card_cvv" => "111",
                  "card_exp_month" => "08",
                  "card_exp_year" => "19",
                  "currency" => "USD",
                  "customer" => "string",
                  "dae" => "string",
                  "description" => "description text",
                  "gpay_token" => "string",
                  "info" => "External information for payments",
                  "ip" => "string",
                  "language" => "uk",
                  "order_id" => "order_id_1",
                  "paytype" => "apay",
                  "phone" => "+380950000001",
                  "prepare" => "1",
                  "public_key" => "string",
                  "recurring" => true,
                  "recurringbytoken" => "1",
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
                  "tavv" => "string",
                  "tid" => "string",
                  "version" => 3
                }} == Jason.decode(body)

        assert {:ok, body_encoded} =
                 Jason.encode(%{
                   "acq_id" => 414_963,
                   "action" => "hold",
                   "agent_commission" => 0.0,
                   "amount" => 0.02,
                   "amount_bonus" => 0.0,
                   "amount_credit" => 0.02,
                   "amount_debit" => 0.02,
                   "authcode_debit" => "542284",
                   "card_token" => "2DFBFE626B734164830DE81E971E90026F260",
                   "commission_credit" => 0.0,
                   "commission_debit" => 0.0,
                   "create_date" => 1_706_750_625_987,
                   "currency" => "UAH",
                   "currency_credit" => "UAH",
                   "currency_debit" => "UAH",
                   "description" => "test",
                   "end_date" => 1_706_750_625_987,
                   "ip" => "string",
                   "is_3ds" => true,
                   "liqpay_order_id" => "NYMK3AE61501685438251925",
                   "mpi_eci" => 7,
                   "order_id" => "98R1U1OV1485849059893399",
                   "payment_id" => 165_162,
                   "paytype" => "card",
                   "public_key" => "i000000000",
                   "receiver_commission" => 0.0,
                   "rrn_debit" => "0006637102400",
                   "sender_bonus" => 0.0,
                   "sender_card_bank" => "pb",
                   "sender_card_country" => 804,
                   "sender_card_mask2" => "414961*99",
                   "sender_card_type" => "visa",
                   "sender_commission" => 0.0,
                   "sender_first_name" => "first name",
                   "sender_last_name" => "last name",
                   "sender_phone" => "380950000001",
                   "status" => "hold_wait",
                   "transaction_id" => 165_162,
                   "type" => "hold",
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
              %LiqPayAPI.InternetAcquiring.TwoStep.Block.Response{
                acq_id: 414_963,
                action: :hold,
                agent_commission: 0.0,
                amount: 0.02,
                amount_bonus: 0.0,
                amount_credit: 0.02,
                amount_debit: 0.02,
                authcode_debit: "542284",
                card_token: "2DFBFE626B734164830DE81E971E90026F260",
                commission_credit: 0.0,
                commission_debit: 0.0,
                create_date: ~U[2024-02-01 01:23:45.987Z],
                currency: "UAH",
                currency_credit: "UAH",
                currency_debit: "UAH",
                description: "test",
                end_date: ~U[2024-02-01 01:23:45.987Z],
                ip: "string",
                is_3ds: true,
                liqpay_order_id: "NYMK3AE61501685438251925",
                mpi_eci: 7,
                order_id: "98R1U1OV1485849059893399",
                payment_id: 165_162,
                paytype: :card,
                public_key: "i000000000",
                receiver_commission: 0.0,
                rrn_debit: "0006637102400",
                sender_bonus: 0.0,
                sender_card_bank: "pb",
                sender_card_country: "804",
                sender_card_mask2: "414961*99",
                sender_card_type: "visa",
                sender_commission: 0.0,
                sender_first_name: "first name",
                sender_last_name: "last name",
                sender_phone: "380950000001",
                status: :hold_wait,
                transaction_id: 165_162,
                type: "hold",
                version: 3
              }} ==
               LiqPayAPI.InternetAcquiring.TwoStep.block(
                 %LiqPayAPI.InternetAcquiring.TwoStep.Block.Request{
                   action: :hold,
                   amount: 5.0,
                   apay_token: "string",
                   card: "4731195301524634",
                   card_cvv: "111",
                   card_exp_month: "08",
                   card_exp_year: "19",
                   currency: :usd,
                   customer: "string",
                   dae: "string",
                   description: "description text",
                   gpay_token: "string",
                   info: "External information for payments",
                   ip: "string",
                   language: :uk,
                   order_id: "order_id_1",
                   paytype: :apay,
                   phone: "+380950000001",
                   prepare: true,
                   public_key: "string",
                   recurring: true,
                   recurringbytoken: true,
                   sender: %LiqPayAPI.InternetAcquiring.TwoStep.Block.Request.Sender{
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
                     %LiqPayAPI.InternetAcquiring.TwoStep.Block.Request.SplitRules{
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
end
