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
                  "split_tickets_only" => true,
                  "version" => 3
                }} == Jason.decode(body)

        assert {:ok, body_encoded} =
                 Jason.encode(%{
                   "acq_id" => 414_963,
                   "action" => "hold",
                   "agent_commission" => 0.0,
                   "amount" => 0.1,
                   "amount_bonus" => 0.02,
                   "amount_credit" => 0.1,
                   "amount_debit" => 0.1,
                   "authcode_credit" => "124257",
                   "authcode_debit" => "805256",
                   "card_token" => "27AA8744A98339BB9E85D50AEB718A93B470395C",
                   "commission_credit" => 0.0,
                   "commission_debit" => 0.0,
                   "completion_date" => 1_715_330_216_463,
                   "confirm_phone" => "380933454182",
                   "create_date" => 1_715_323_287_407,
                   "currency" => "UAH",
                   "currency_credit" => "UAH",
                   "currency_debit" => "UAH",
                   "description" => "testing pay by card",
                   "end_date" => 1_715_330_217_449,
                   "ip" => "8.8.0.0",
                   "is_3ds" => true,
                   "language" => "uk",
                   "liqpay_order_id" => "YHVDEMXJ1715323369444359",
                   "mpi_eci" => 7,
                   "order_id" => "idByCard345D308",
                   "payment_id" => 13_291_299,
                   "paytype" => "card",
                   "public_key" => "i16202663459",
                   "receiver_commission" => 0.0,
                   "result" => "ok",
                   "rrn_credit" => "000000454715",
                   "rrn_debit" => "000000454153",
                   "sender_bonus" => 0.02,
                   "sender_card_bank" => "pb",
                   "sender_card_country" => 804,
                   "sender_card_mask2" => "545708*24",
                   "sender_card_type" => "mc",
                   "sender_commission" => 0.0,
                   "sender_first_name" => "Тетяна",
                   "sender_last_name" => "Станько",
                   "sender_phone" => "380933454182",
                   "status" => "success",
                   "transaction_id" => 13_291_299,
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
                amount: 0.1,
                amount_bonus: 0.02,
                amount_credit: 0.1,
                amount_debit: 0.1,
                authcode_credit: "124257",
                authcode_debit: "805256",
                card_token: "27AA8744A98339BB9E85D50AEB718A93B470395C",
                commission_credit: 0.0,
                commission_debit: 0.0,
                completion_date: ~U[2024-05-10 08:36:56.463Z],
                confirm_phone: "380933454182",
                create_date: ~U[2024-05-10 06:41:27.407Z],
                currency: "UAH",
                currency_credit: "UAH",
                currency_debit: "UAH",
                description: "testing pay by card",
                end_date: ~U[2024-05-10 08:36:57.449Z],
                ip: "8.8.0.0",
                is_3ds: true,
                language: "uk",
                liqpay_order_id: "YHVDEMXJ1715323369444359",
                mpi_eci: 7,
                order_id: "idByCard345D308",
                payment_id: 13_291_299,
                paytype: :card,
                public_key: "i16202663459",
                receiver_commission: 0.0,
                result: :ok,
                rrn_credit: "000000454715",
                rrn_debit: "000000454153",
                sender_bonus: 0.02,
                sender_card_bank: "pb",
                sender_card_country: 804,
                sender_card_mask2: "545708*24",
                sender_card_type: "mc",
                sender_commission: 0.0,
                sender_first_name: "Тетяна",
                sender_last_name: "Станько",
                sender_phone: "380933454182",
                status: :success,
                transaction_id: 13_291_299,
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
                   split_tickets_only: true,
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
                  "split_rules" =>
                    "[{\"amount\":404,\"commission_payer\":\"sender\",\"description\":\"string\",\"public_key\":\"i000000001\",\"rro_info\":{\"delivery_emails\":[\"string\"],\"items\":[{\"amount\":2,\"cost\":404,\"id\":123456,\"price\":202}]},\"server_url\":\"https://server1/callback\"}]",
                  "tavv" => "string",
                  "tid" => "string",
                  "version" => 3
                }} == Jason.decode(body)

        assert {:ok, body_encoded} =
                 Jason.encode(%{
                   "acq_id" => 414_963,
                   "action" => "hold",
                   "agent_commission" => 0.0,
                   "amount" => 0.1,
                   "amount_bonus" => 0.02,
                   "amount_credit" => 0.1,
                   "amount_debit" => 0.1,
                   "authcode_debit" => "805256",
                   "card_token" => "27AA8744A98339BB9E85D50AEB718A93B470395C",
                   "commission_credit" => 0.0,
                   "commission_debit" => 0.0,
                   "confirm_phone" => "380933454182",
                   "create_date" => 1_715_323_287_407,
                   "currency" => "UAH",
                   "currency_credit" => "UAH",
                   "currency_debit" => "UAH",
                   "description" => "testing pay by card",
                   "end_date" => 1_706_750_625_987,
                   "ip" => "8.8.0.0",
                   "is_3ds" => true,
                   "language" => "uk",
                   "liqpay_order_id" => "YHVDEMXJ1715323369444359",
                   "mpi_eci" => 7,
                   "order_id" => "idByCard345D308",
                   "payment_id" => 13_291_299,
                   "paytype" => "card",
                   "public_key" => "i16202663459",
                   "receiver_commission" => 0.0,
                   "result" => "ok",
                   "rrn_debit" => "000000454153",
                   "sender_bonus" => 0.02,
                   "sender_card_bank" => "pb",
                   "sender_card_country" => 804,
                   "sender_card_mask2" => "545708*24",
                   "sender_card_type" => "mc",
                   "sender_commission" => 0.0,
                   "sender_first_name" => "Тетяна",
                   "sender_last_name" => "Станько",
                   "sender_phone" => "380933454182",
                   "status" => "hold_wait",
                   "transaction_id" => 13_291_299,
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
                amount: 0.1,
                amount_bonus: 0.02,
                amount_credit: 0.1,
                amount_debit: 0.1,
                authcode_debit: "805256",
                card_token: "27AA8744A98339BB9E85D50AEB718A93B470395C",
                commission_credit: 0.0,
                commission_debit: 0.0,
                confirm_phone: "380933454182",
                create_date: ~U[2024-05-10 06:41:27.407Z],
                currency: "UAH",
                currency_credit: "UAH",
                currency_debit: "UAH",
                description: "testing pay by card",
                end_date: ~U[2024-02-01 01:23:45.987Z],
                ip: "8.8.0.0",
                is_3ds: true,
                language: "uk",
                liqpay_order_id: "YHVDEMXJ1715323369444359",
                mpi_eci: 7,
                order_id: "idByCard345D308",
                payment_id: 13_291_299,
                paytype: :card,
                public_key: "i16202663459",
                receiver_commission: 0.0,
                result: :ok,
                rrn_debit: "000000454153",
                sender_bonus: 0.02,
                sender_card_bank: "pb",
                sender_card_country: 804,
                sender_card_mask2: "545708*24",
                sender_card_type: "mc",
                sender_commission: 0.0,
                sender_first_name: "Тетяна",
                sender_last_name: "Станько",
                sender_phone: "380933454182",
                status: :hold_wait,
                transaction_id: 13_291_299,
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
                       amount: 404,
                       commission_payer: :sender,
                       description: "string",
                       public_key: "i000000001",
                       rro_info:
                         %LiqPayAPI.InternetAcquiring.TwoStep.Block.Request.SplitRulesRroInfo{
                           delivery_emails: ["string"],
                           items: [
                             %LiqPayAPI.InternetAcquiring.TwoStep.Block.Request.SplitRulesRroInfoItems{
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
