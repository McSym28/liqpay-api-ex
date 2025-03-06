defmodule LiqPayAPI.ConfirmationTest do
  use ExUnit.Case, async: true
  import Mox

  @httpoison OpenAPIClient.HTTPoisonMock
  @client OpenAPIClientMock

  setup :verify_on_exit!

  describe "sender_verify/2" do
    test "[200] performs a request, encodes SenderVerify.Request from request's body and decodes SenderVerify.Response from response's body" do
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
                  "action" => "confirm",
                  "confirm_token" => "JmV2qDgC3LotB8njGDcEYjHFpmpAZR",
                  "public_key" => "string",
                  "sender_address" => "address",
                  "sender_city" => "city",
                  "sender_country_code" => "code",
                  "sender_first_name" => "first name",
                  "sender_last_name" => "last name",
                  "sender_postal_code" => "string",
                  "version" => 3
                }} == Jason.decode(body)

        assert {:ok, body_encoded} =
                 Jason.encode(%{
                   "acq_id" => 414_963,
                   "action" => "p2p",
                   "agent_commission" => 0.0,
                   "amount" => 1.0,
                   "amount_bonus" => 0.0,
                   "amount_credit" => 0.04,
                   "amount_debit" => 1.0,
                   "code" => "err_payment",
                   "commission_credit" => 0.0,
                   "commission_debit" => 0.02,
                   "create_date" => 1_501_754_644_818,
                   "currency" => "UAH",
                   "currency_credit" => "USD",
                   "currency_debit" => "UAH",
                   "description" => "p2p",
                   "end_date" => 1_501_754_644_818,
                   "err_code" => "err_payment",
                   "err_description" => "Error payment",
                   "is_3ds" => true,
                   "liqpay_order_id" => "NYMK3AE61501685438251925",
                   "mpi_eci" => 7,
                   "order_id" => "98R1U1OV1485849059893399",
                   "payment_id" => 165_235,
                   "paytype" => "card",
                   "public_key" => "i000000000",
                   "receiver_commission" => 0.0,
                   "sender_bonus" => 0.0,
                   "sender_card_bank" => "pb",
                   "sender_card_country" => 804,
                   "sender_card_mask2" => "414962*99",
                   "sender_card_type" => "visa",
                   "sender_commission" => 0.02,
                   "sender_first_name" => "test",
                   "sender_last_name" => "string",
                   "sender_phone" => "380950000001",
                   "status" => "failure",
                   "transaction_id" => 165_235,
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
              %LiqPayAPI.Confirmation.SenderVerify.Response{
                acq_id: 414_963,
                action: "p2p",
                agent_commission: 0.0,
                amount: 1.0,
                amount_bonus: 0.0,
                amount_credit: 0.04,
                amount_debit: 1.0,
                code: "err_payment",
                commission_credit: 0.0,
                commission_debit: 0.02,
                create_date: ~U[2017-08-03 10:04:04.818Z],
                currency: "UAH",
                currency_credit: "USD",
                currency_debit: "UAH",
                description: "p2p",
                end_date: ~U[2017-08-03 10:04:04.818Z],
                err_code: "err_payment",
                err_description: "Error payment",
                is_3ds: true,
                liqpay_order_id: "NYMK3AE61501685438251925",
                mpi_eci: 7,
                order_id: "98R1U1OV1485849059893399",
                payment_id: 165_235,
                paytype: :card,
                public_key: "i000000000",
                receiver_commission: 0.0,
                sender_bonus: 0.0,
                sender_card_bank: "pb",
                sender_card_country: "804",
                sender_card_mask2: "414962*99",
                sender_card_type: "visa",
                sender_commission: 0.02,
                sender_first_name: "test",
                sender_last_name: "string",
                sender_phone: "380950000001",
                status: :failure,
                transaction_id: 165_235,
                version: 3
              }} ==
               LiqPayAPI.Confirmation.sender_verify(
                 %LiqPayAPI.Confirmation.SenderVerify.Request{
                   action: :confirm,
                   confirm_token: "JmV2qDgC3LotB8njGDcEYjHFpmpAZR",
                   public_key: "string",
                   sender_address: "address",
                   sender_city: "city",
                   sender_country_code: "code",
                   sender_first_name: "first name",
                   sender_last_name: "last name",
                   sender_postal_code: "string",
                   version: 3
                 },
                 private_key: "a4825234f4bae72a0be04eafe9e8e2bada209255",
                 base_url: "https://example.com"
               )
    end
  end

  describe "mpi/2" do
    test "[200] performs a request, encodes MPI.Request from request's body and decodes MPI.Response from response's body" do
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
                  "action" => "mpi",
                  "action_payment" => "pay",
                  "amount" => 5.0,
                  "card" => "4731195301524634",
                  "card_cvv" => "111",
                  "card_exp_month" => "08",
                  "card_exp_year" => "19",
                  "currency" => "USD",
                  "description" => "description text",
                  "email" => "string",
                  "ip" => "string",
                  "language" => "uk",
                  "order_id" => "order_id_1",
                  "phone" => "380950000001",
                  "public_key" => "string",
                  "sender_first_name" => "string",
                  "sender_last_name" => "string",
                  "threeDSInfo" => %{
                    "browserAcceptHeader" => "string",
                    "browserColorDepth" => "string",
                    "browserJavaEnabled" => false,
                    "browserJavascriptEnabled" => true,
                    "browserLanguage" => "en-US",
                    "browserScreenHeight" => "string",
                    "browserScreenWidth" => "string",
                    "browserTZ" => "300",
                    "browserUserAgent" => "string",
                    "notificationURL" => "string",
                    "threeDSRequestorURL" => "string"
                  },
                  "version" => 3
                }} == Jason.decode(body)

        assert {:ok, body_encoded} =
                 Jason.encode(%{
                   "cres" =>
                     "eyJhY3NUcmFuc0lEIjoiYTc1ZGYxZGYtZmRhZi00YzcyLTlkOTItYTM1NDUzMzEzODU1IiwibWVzc2FnZVR5cGUiOiJDUmVzIiwibWVzc2FnZVZlcnNpb24iOiIyLjEuMCIsInRocmVlRFNTZXJ2ZXJUcmFuc0lEIjoiYTQyNzQwNmUtZDlmOS00MmI2LTk5MDItOGQ2OTZjMGVkNjJjIiwidHJhbnNTdGF0dXMiOiJBIn0",
                   "mpi_cres" =>
                     "eyJhY3NUcmFuc0lEIjoiYTc1ZGYxZGYtZmRhZi00YzcyLTlkOTItYTM1NDUzMzEzODU1IiwibWVzc2FnZVR5cGUiOiJDUmVzIiwibWVzc2FnZVZlcnNpb24iOiIyLjEuMCIsInRocmVlRFNTZXJ2ZXJUcmFuc0lEIjoiYTQyNzQwNmUtZDlmOS00MmI2LTk5MDItOGQ2OTZjMGVkNjJjIiwidHJhbnNTdGF0dXMiOiJBIn0",
                   "mpi_form" =>
                     "< form action = 'https://acs4.privatbank.ua/acs/creq' method = 'post' > < input type = 'hidden' name = 'creq' value = 'eyJhY3NUcmFuc0lEIjoiNDJlYmRkOWMtNzU0Zi00YzMwLTg5NDMtNDJhODQzMDY4ZTMwIiwibWVzc2FnZVR5cGUiOiJDUmVxIiwibWVzc2FnZVZlcnNpb24iOiIyLjEuMCIsInRocmVlRFNTZXJ2ZXJUcmFuc0lEIjoiZTc1YmEyOGItNjE0YS00NDE5LWFjMzQtNzkxZDlmMjkwZjE3IiwiY2hhbGxlbmdlV2luZG93U2l6ZSI6IjAyIn0=' / > < /form>",
                   "mpi_req_md" => "MGUyZDYxMjYDfhKOC00MDI2LWFhMWUOpDFJjNzIyMmDffShZjli",
                   "mpi_req_pareq" =>
                     "eJxVUe1OwjAUfRWyB1jbMWAjlybIjJI4J1454B/TlKuUsA+6zTCf3lvYRPvrnJN72tNz4XlrEZMn
1I1FCSlWlfrgdnMvNX8fTyKAx7xwJOwmj/iQcIX2soUuRQ+9wNgPSWj1VV1xKUPlwt10Uowng8
BNZRyNAuEyl4d2IRjfgI2FmGXGUovQ/jATtB0EWT17aVEQ+B9QQ7ld5fbui6rKWOoi4zcGv29OZSq
9Rt10VhWGorn5oFdsq0ahyq6/2g2Mr15ad+S9ret0l34/LDhPk2Vw93o9A+YmYKNqlAEXEx4J
MRBiOpMQ4p90kFlLpjkPhf00T0r0xPxOS/1Kgki3mupVxxOlbPQM8lkWNEGZfzGwS9zFrStW
19RVvxNX7UlwbkPtiLGL0ETFgzsK6rbFur4T+7fsHPamnMg==",
                   "mpi_req_url" => "https://acs.bankname.com/mpi",
                   "mpi_status" => "Y",
                   "mpi_version" => "2.0",
                   "result" => "ok",
                   "status" => "success"
                 })

        {:ok,
         %HTTPoison.Response{
           status_code: 200,
           headers: [{"Content-Type", "application/json"}],
           body: body_encoded
         }}
      end)

      assert {:ok,
              %LiqPayAPI.Confirmation.MPI.Response{
                cres:
                  "eyJhY3NUcmFuc0lEIjoiYTc1ZGYxZGYtZmRhZi00YzcyLTlkOTItYTM1NDUzMzEzODU1IiwibWVzc2FnZVR5cGUiOiJDUmVzIiwibWVzc2FnZVZlcnNpb24iOiIyLjEuMCIsInRocmVlRFNTZXJ2ZXJUcmFuc0lEIjoiYTQyNzQwNmUtZDlmOS00MmI2LTk5MDItOGQ2OTZjMGVkNjJjIiwidHJhbnNTdGF0dXMiOiJBIn0",
                mpi_cres:
                  "eyJhY3NUcmFuc0lEIjoiYTc1ZGYxZGYtZmRhZi00YzcyLTlkOTItYTM1NDUzMzEzODU1IiwibWVzc2FnZVR5cGUiOiJDUmVzIiwibWVzc2FnZVZlcnNpb24iOiIyLjEuMCIsInRocmVlRFNTZXJ2ZXJUcmFuc0lEIjoiYTQyNzQwNmUtZDlmOS00MmI2LTk5MDItOGQ2OTZjMGVkNjJjIiwidHJhbnNTdGF0dXMiOiJBIn0",
                mpi_form:
                  "< form action = 'https://acs4.privatbank.ua/acs/creq' method = 'post' > < input type = 'hidden' name = 'creq' value = 'eyJhY3NUcmFuc0lEIjoiNDJlYmRkOWMtNzU0Zi00YzMwLTg5NDMtNDJhODQzMDY4ZTMwIiwibWVzc2FnZVR5cGUiOiJDUmVxIiwibWVzc2FnZVZlcnNpb24iOiIyLjEuMCIsInRocmVlRFNTZXJ2ZXJUcmFuc0lEIjoiZTc1YmEyOGItNjE0YS00NDE5LWFjMzQtNzkxZDlmMjkwZjE3IiwiY2hhbGxlbmdlV2luZG93U2l6ZSI6IjAyIn0=' / > < /form>",
                mpi_req_md: "MGUyZDYxMjYDfhKOC00MDI2LWFhMWUOpDFJjNzIyMmDffShZjli",
                mpi_req_pareq:
                  "eJxVUe1OwjAUfRWyB1jbMWAjlybIjJI4J1454B/TlKuUsA+6zTCf3lvYRPvrnJN72tNz4XlrEZMn
1I1FCSlWlfrgdnMvNX8fTyKAx7xwJOwmj/iQcIX2soUuRQ+9wNgPSWj1VV1xKUPlwt10Uowng8
BNZRyNAuEyl4d2IRjfgI2FmGXGUovQ/jATtB0EWT17aVEQ+B9QQ7ld5fbui6rKWOoi4zcGv29OZSq
9Rt10VhWGorn5oFdsq0ahyq6/2g2Mr15ad+S9ret0l34/LDhPk2Vw93o9A+YmYKNqlAEXEx4J
MRBiOpMQ4p90kFlLpjkPhf00T0r0xPxOS/1Kgki3mupVxxOlbPQM8lkWNEGZfzGwS9zFrStW
19RVvxNX7UlwbkPtiLGL0ETFgzsK6rbFur4T+7fsHPamnMg==",
                mpi_req_url: "https://acs.bankname.com/mpi",
                mpi_status: :y,
                mpi_version: :"2/0",
                result: :ok,
                status: :success
              }} ==
               LiqPayAPI.Confirmation.mpi(
                 %LiqPayAPI.Confirmation.MPI.Request{
                   action: :mpi,
                   action_payment: :pay,
                   amount: 5.0,
                   card: "4731195301524634",
                   card_cvv: "111",
                   card_exp_month: "08",
                   card_exp_year: "19",
                   currency: :usd,
                   description: "description text",
                   email: "string",
                   ip: "string",
                   language: :uk,
                   order_id: "order_id_1",
                   phone: "380950000001",
                   public_key: "string",
                   sender_first_name: "string",
                   sender_last_name: "string",
                   three_ds_info: %LiqPayAPI.Confirmation.MPI.Request.ThreeDSInfo{
                     browser_accept_header: "string",
                     browser_color_depth: "string",
                     browser_java_enabled: false,
                     browser_javascript_enabled: true,
                     browser_language: "en-US",
                     browser_screen_height: "string",
                     browser_screen_width: "string",
                     browser_tz: "300",
                     browser_user_agent: "string",
                     notification_url: "string",
                     three_ds_requestor_url: "string"
                   },
                   version: 3
                 },
                 private_key: "a4825234f4bae72a0be04eafe9e8e2bada209255",
                 base_url: "https://example.com"
               )
    end
  end

  describe "three_ds/2" do
    test "[200] performs a request, encodes ThreeDS.Request from request's body and decodes ThreeDS.Response from response's body" do
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
                  "action" => "confirm",
                  "confirm_token" => "JmV2qDgC3LotB8njGDcEYjHFpmpAZR",
                  "public_key" => "string",
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
                   "authcode_credit" => "504979",
                   "authcode_debit" => "988083",
                   "bonus_procent" => 7.0,
                   "bonus_type" => "bonusplus",
                   "card_token" => "2DFBFE626B734161130DE81E971E948D6F260",
                   "commission_credit" => 0.0,
                   "commission_debit" => 0.0,
                   "create_date" => 1_501_688_827_926,
                   "currency" => "UAH",
                   "currency_credit" => "UAH",
                   "currency_debit" => "UAH",
                   "description" => "description",
                   "end_date" => 1_501_688_827_926,
                   "ip" => "8.8.8.8",
                   "is_3ds" => true,
                   "liqpay_order_id" => "NYMK3AE61501685438251925",
                   "mpi_cres" => "string",
                   "mpi_eci" => 5,
                   "order_id" => "98R1U1OV1485849059893399",
                   "payment_id" => 165_193,
                   "paytype" => "card",
                   "public_key" => "i000000000",
                   "receiver_commission" => 0.0,
                   "rrn_credit" => "000663782985",
                   "rrn_debit" => "000663782982",
                   "sender_bonus" => 0.0,
                   "sender_card_bank" => "pb",
                   "sender_card_country" => 804,
                   "sender_card_mask2" => "473118*55",
                   "sender_card_type" => "visa",
                   "sender_commission" => 0.0,
                   "sender_first_name" => "first name",
                   "sender_last_name" => "last name",
                   "sender_phone" => "380950000001",
                   "status" => "success",
                   "transaction_id" => 165_193,
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
              %LiqPayAPI.Confirmation.ThreeDS.Response{
                acq_id: 414_963,
                action: :pay,
                agent_commission: 0.0,
                amount: 0.02,
                amount_bonus: 0.0,
                amount_credit: 0.02,
                amount_debit: 0.02,
                authcode_credit: "504979",
                authcode_debit: "988083",
                bonus_procent: 7.0,
                bonus_type: :bonusplus,
                card_token: "2DFBFE626B734161130DE81E971E948D6F260",
                commission_credit: 0.0,
                commission_debit: 0.0,
                create_date: ~U[2017-08-02 15:47:07.926Z],
                currency: "UAH",
                currency_credit: "UAH",
                currency_debit: "UAH",
                description: "description",
                end_date: ~U[2017-08-02 15:47:07.926Z],
                ip: "8.8.8.8",
                is_3ds: true,
                liqpay_order_id: "NYMK3AE61501685438251925",
                mpi_cres: "string",
                mpi_eci: 5,
                order_id: "98R1U1OV1485849059893399",
                payment_id: 165_193,
                paytype: :card,
                public_key: "i000000000",
                receiver_commission: 0.0,
                rrn_credit: "000663782985",
                rrn_debit: "000663782982",
                sender_bonus: 0.0,
                sender_card_bank: "pb",
                sender_card_country: "804",
                sender_card_mask2: "473118*55",
                sender_card_type: "visa",
                sender_commission: 0.0,
                sender_first_name: "first name",
                sender_last_name: "last name",
                sender_phone: "380950000001",
                status: :success,
                transaction_id: 165_193,
                type: "buy",
                version: 3
              }} ==
               LiqPayAPI.Confirmation.three_ds(
                 %LiqPayAPI.Confirmation.ThreeDS.Request{
                   action: :confirm,
                   confirm_token: "JmV2qDgC3LotB8njGDcEYjHFpmpAZR",
                   public_key: "string",
                   version: 3
                 },
                 private_key: "a4825234f4bae72a0be04eafe9e8e2bada209255",
                 base_url: "https://example.com"
               )
    end
  end

  describe "receiver_verify/2" do
    test "[200] performs a request, encodes ReceiverVerify.Request from request's body and decodes ReceiverVerify.Response from response's body" do
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
                  "action" => "confirm",
                  "confirm_token" => "JmV2qDgC3LotB8njGDcEYjHFpmpAZR",
                  "public_key" => "string",
                  "receiver_first_name" => "first name",
                  "receiver_last_name" => "last name",
                  "version" => 3
                }} == Jason.decode(body)

        assert {:ok, body_encoded} =
                 Jason.encode(%{
                   "acq_id" => 414_963,
                   "action" => "p2p",
                   "agent_commission" => 0.0,
                   "amount" => 1.0,
                   "amount_bonus" => 0.0,
                   "amount_credit" => 0.04,
                   "amount_debit" => 1.0,
                   "code" => "err_payment",
                   "commission_credit" => 0.0,
                   "commission_debit" => 0.02,
                   "create_date" => 1_501_754_644_818,
                   "currency" => "UAH",
                   "currency_credit" => "USD",
                   "currency_debit" => "UAH",
                   "description" => "p2p",
                   "end_date" => 1_501_754_644_818,
                   "err_code" => "err_payment",
                   "err_description" => "Error payment",
                   "is_3ds" => true,
                   "liqpay_order_id" => "NYMK3AE61501685438251925",
                   "mpi_eci" => 7,
                   "order_id" => "98R1U1OV1485849059893399",
                   "payment_id" => 165_235,
                   "paytype" => "card",
                   "public_key" => "i000000000",
                   "receiver_commission" => 0.0,
                   "sender_bonus" => 0.0,
                   "sender_card_bank" => "pb",
                   "sender_card_country" => 804,
                   "sender_card_mask2" => "414962*99",
                   "sender_card_type" => "visa",
                   "sender_commission" => 0.02,
                   "sender_first_name" => "test",
                   "sender_last_name" => "string",
                   "sender_phone" => "380950000001",
                   "status" => "failure",
                   "transaction_id" => 165_235,
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
              %LiqPayAPI.Confirmation.ReceiverVerify.Response{
                acq_id: 414_963,
                action: "p2p",
                agent_commission: 0.0,
                amount: 1.0,
                amount_bonus: 0.0,
                amount_credit: 0.04,
                amount_debit: 1.0,
                code: "err_payment",
                commission_credit: 0.0,
                commission_debit: 0.02,
                create_date: ~U[2017-08-03 10:04:04.818Z],
                currency: "UAH",
                currency_credit: "USD",
                currency_debit: "UAH",
                description: "p2p",
                end_date: ~U[2017-08-03 10:04:04.818Z],
                err_code: "err_payment",
                err_description: "Error payment",
                is_3ds: true,
                liqpay_order_id: "NYMK3AE61501685438251925",
                mpi_eci: 7,
                order_id: "98R1U1OV1485849059893399",
                payment_id: 165_235,
                paytype: :card,
                public_key: "i000000000",
                receiver_commission: 0.0,
                sender_bonus: 0.0,
                sender_card_bank: "pb",
                sender_card_country: "804",
                sender_card_mask2: "414962*99",
                sender_card_type: "visa",
                sender_commission: 0.02,
                sender_first_name: "test",
                sender_last_name: "string",
                sender_phone: "380950000001",
                status: :failure,
                transaction_id: 165_235,
                version: 3
              }} ==
               LiqPayAPI.Confirmation.receiver_verify(
                 %LiqPayAPI.Confirmation.ReceiverVerify.Request{
                   action: :confirm,
                   confirm_token: "JmV2qDgC3LotB8njGDcEYjHFpmpAZR",
                   public_key: "string",
                   receiver_first_name: "first name",
                   receiver_last_name: "last name",
                   version: 3
                 },
                 private_key: "a4825234f4bae72a0be04eafe9e8e2bada209255",
                 base_url: "https://example.com"
               )
    end
  end

  describe "otp/2" do
    test "[200] performs a request, encodes OTP.Request from request's body and decodes OTP.Response from response's body" do
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
                  "action" => "confirm",
                  "confirm_token" => "JmV2qDgC3LotB8njGDcEYjHFpmpAZR",
                  "otp" => "12345678",
                  "public_key" => "string",
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
                   "authcode_credit" => "504979",
                   "authcode_debit" => "988083",
                   "bonus_procent" => 7.0,
                   "bonus_type" => "bonusplus",
                   "card_token" => "2DFBFE626B734161130DE81E971E948D6F260",
                   "commission_credit" => 0.0,
                   "commission_debit" => 0.0,
                   "create_date" => 1_501_688_827_926,
                   "currency" => "UAH",
                   "currency_credit" => "UAH",
                   "currency_debit" => "UAH",
                   "description" => "description",
                   "end_date" => 1_501_688_827_926,
                   "ip" => "8.8.8.8",
                   "is_3ds" => true,
                   "liqpay_order_id" => "NYMK3AE61501685438251925",
                   "mpi_eci" => 5,
                   "order_id" => "98R1U1OV1485849059893399",
                   "payment_id" => 165_193,
                   "paytype" => "card",
                   "public_key" => "i000000000",
                   "receiver_commission" => 0.0,
                   "rrn_credit" => "000663782985",
                   "rrn_debit" => "000663782982",
                   "sender_bonus" => 0.0,
                   "sender_card_bank" => "pb",
                   "sender_card_country" => 804,
                   "sender_card_mask2" => "473118*55",
                   "sender_card_type" => "visa",
                   "sender_commission" => 0.0,
                   "sender_first_name" => "first name",
                   "sender_last_name" => "last name",
                   "sender_phone" => "380950000001",
                   "status" => "success",
                   "transaction_id" => 165_193,
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
              %LiqPayAPI.Confirmation.OTP.Response{
                acq_id: 414_963,
                action: :pay,
                agent_commission: 0.0,
                amount: 0.02,
                amount_bonus: 0.0,
                amount_credit: 0.02,
                amount_debit: 0.02,
                authcode_credit: "504979",
                authcode_debit: "988083",
                bonus_procent: 7.0,
                bonus_type: :bonusplus,
                card_token: "2DFBFE626B734161130DE81E971E948D6F260",
                commission_credit: 0.0,
                commission_debit: 0.0,
                create_date: ~U[2017-08-02 15:47:07.926Z],
                currency: "UAH",
                currency_credit: "UAH",
                currency_debit: "UAH",
                description: "description",
                end_date: ~U[2017-08-02 15:47:07.926Z],
                ip: "8.8.8.8",
                is_3ds: true,
                liqpay_order_id: "NYMK3AE61501685438251925",
                mpi_eci: 5,
                order_id: "98R1U1OV1485849059893399",
                payment_id: 165_193,
                paytype: :card,
                public_key: "i000000000",
                receiver_commission: 0.0,
                rrn_credit: "000663782985",
                rrn_debit: "000663782982",
                sender_bonus: 0.0,
                sender_card_bank: "pb",
                sender_card_country: "804",
                sender_card_mask2: "473118*55",
                sender_card_type: "visa",
                sender_commission: 0.0,
                sender_first_name: "first name",
                sender_last_name: "last name",
                sender_phone: "380950000001",
                status: :success,
                transaction_id: 165_193,
                type: "buy",
                version: 3
              }} ==
               LiqPayAPI.Confirmation.otp(
                 %LiqPayAPI.Confirmation.OTP.Request{
                   action: :confirm,
                   confirm_token: "JmV2qDgC3LotB8njGDcEYjHFpmpAZR",
                   otp: "12345678",
                   public_key: "string",
                   version: 3
                 },
                 private_key: "a4825234f4bae72a0be04eafe9e8e2bada209255",
                 base_url: "https://example.com"
               )
    end
  end

  describe "card_verification/2" do
    test "[200] performs a request, encodes CardVerification.Request from request's body and decodes CardVerification.Response from response's body" do
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
                  "action" => "cardverification",
                  "card" => "string",
                  "card_cvv" => "string",
                  "card_exp_month" => "08",
                  "card_exp_year" => "19",
                  "description" => "string",
                  "ip" => "string",
                  "language" => "uk",
                  "order_id" => "string",
                  "public_key" => "string",
                  "verifycode" => "Y",
                  "version" => 3
                }} == Jason.decode(body)

        assert {:ok, body_encoded} =
                 Jason.encode(%{
                   "acq_id" => 414_963,
                   "action" => "cardverification",
                   "agent_commission" => 0.0,
                   "amount" => 0,
                   "amount_bonus" => 0.0,
                   "amount_credit" => 0,
                   "amount_debit" => 0,
                   "authcode_credit" => "string",
                   "authcode_debit" => "559310",
                   "commission_credit" => 0.0,
                   "commission_debit" => 0.0,
                   "confirm_phone" => "380950000001",
                   "create_date" => 1_501_687_647_977,
                   "currency" => "UAH",
                   "currency_credit" => "UAH",
                   "currency_debit" => "UAH",
                   "description" => "тест",
                   "end_date" => 1_501_687_647_977,
                   "ip" => "string",
                   "is_3ds" => true,
                   "liqpay_order_id" => "NYMK3AE61501685438251925",
                   "mpi_eci" => 7,
                   "order_id" => "98R1U1OV1485849059893399",
                   "payment_id" => 165_192,
                   "paytype" => "card",
                   "public_key" => "i000000000",
                   "receiver_commission" => 0.0,
                   "rrn_debit" => "000663770891",
                   "sender_bonus" => 0.0,
                   "sender_card_bank" => "pb",
                   "sender_card_country" => 804,
                   "sender_card_mask2" => "414962*99",
                   "sender_card_type" => "visa",
                   "sender_commission" => 0.0,
                   "sender_first_name" => "string",
                   "sender_last_name" => "string",
                   "sender_phone" => "380950000001",
                   "status" => "success",
                   "transaction_id" => 165_192,
                   "type" => "string",
                   "verifycode" => "S8SBQM",
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
              %LiqPayAPI.Confirmation.CardVerification.Response{
                acq_id: 414_963,
                action: :cardverification,
                agent_commission: 0.0,
                amount: 0,
                amount_bonus: 0.0,
                amount_credit: 0,
                amount_debit: 0,
                authcode_credit: "string",
                authcode_debit: "559310",
                commission_credit: 0.0,
                commission_debit: 0.0,
                confirm_phone: "380950000001",
                create_date: ~U[2017-08-02 15:27:27.977Z],
                currency: "UAH",
                currency_credit: "UAH",
                currency_debit: "UAH",
                description: "тест",
                end_date: ~U[2017-08-02 15:27:27.977Z],
                ip: "string",
                is_3ds: true,
                liqpay_order_id: "NYMK3AE61501685438251925",
                mpi_eci: 7,
                order_id: "98R1U1OV1485849059893399",
                payment_id: 165_192,
                paytype: :card,
                public_key: "i000000000",
                receiver_commission: 0.0,
                rrn_debit: "000663770891",
                sender_bonus: 0.0,
                sender_card_bank: "pb",
                sender_card_country: "804",
                sender_card_mask2: "414962*99",
                sender_card_type: "visa",
                sender_commission: 0.0,
                sender_first_name: "string",
                sender_last_name: "string",
                sender_phone: "380950000001",
                status: :success,
                transaction_id: 165_192,
                type: "string",
                verifycode: "S8SBQM",
                version: 3
              }} ==
               LiqPayAPI.Confirmation.card_verification(
                 %LiqPayAPI.Confirmation.CardVerification.Request{
                   action: :cardverification,
                   card: "string",
                   card_cvv: "string",
                   card_exp_month: "08",
                   card_exp_year: "19",
                   description: "string",
                   ip: "string",
                   language: :uk,
                   order_id: "string",
                   public_key: "string",
                   verifycode: true,
                   version: 3
                 },
                 private_key: "a4825234f4bae72a0be04eafe9e8e2bada209255",
                 base_url: "https://example.com"
               )
    end
  end

  describe "cvv/2" do
    test "[200] performs a request, encodes CVV.Request from request's body and decodes CVV.Response from response's body" do
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
                  "action" => "confirm",
                  "card_cvv" => "123",
                  "confirm_token" => "JmV2qDgC3LotB8njGDcEYjHFpmpAZR",
                  "public_key" => "string",
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
                   "authcode_credit" => "504979",
                   "authcode_debit" => "988083",
                   "bonus_procent" => 7.0,
                   "bonus_type" => "bonusplus",
                   "card_token" => "2DFBFE626B734161130DE81E971E948D6F260",
                   "commission_credit" => 0.0,
                   "commission_debit" => 0.0,
                   "create_date" => 1_501_688_827_926,
                   "currency" => "UAH",
                   "currency_credit" => "UAH",
                   "currency_debit" => "UAH",
                   "description" => "description",
                   "end_date" => 1_501_688_827_926,
                   "ip" => "8.8.8.8",
                   "is_3ds" => true,
                   "liqpay_order_id" => "NYMK3AE61501685438251925",
                   "mpi_eci" => 5,
                   "order_id" => "98R1U1OV1485849059893399",
                   "payment_id" => 165_193,
                   "paytype" => "card",
                   "public_key" => "i000000000",
                   "receiver_commission" => 0.0,
                   "rrn_credit" => "000663782985",
                   "rrn_debit" => "000663782982",
                   "sender_bonus" => 0.0,
                   "sender_card_bank" => "pb",
                   "sender_card_country" => 804,
                   "sender_card_mask2" => "473118*55",
                   "sender_card_type" => "visa",
                   "sender_commission" => 0.0,
                   "sender_first_name" => "first name",
                   "sender_last_name" => "last name",
                   "sender_phone" => "380950000001",
                   "status" => "success",
                   "transaction_id" => 165_193,
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
              %LiqPayAPI.Confirmation.CVV.Response{
                acq_id: 414_963,
                action: :pay,
                agent_commission: 0.0,
                amount: 0.02,
                amount_bonus: 0.0,
                amount_credit: 0.02,
                amount_debit: 0.02,
                authcode_credit: "504979",
                authcode_debit: "988083",
                bonus_procent: 7.0,
                bonus_type: :bonusplus,
                card_token: "2DFBFE626B734161130DE81E971E948D6F260",
                commission_credit: 0.0,
                commission_debit: 0.0,
                create_date: ~U[2017-08-02 15:47:07.926Z],
                currency: "UAH",
                currency_credit: "UAH",
                currency_debit: "UAH",
                description: "description",
                end_date: ~U[2017-08-02 15:47:07.926Z],
                ip: "8.8.8.8",
                is_3ds: true,
                liqpay_order_id: "NYMK3AE61501685438251925",
                mpi_eci: 5,
                order_id: "98R1U1OV1485849059893399",
                payment_id: 165_193,
                paytype: :card,
                public_key: "i000000000",
                receiver_commission: 0.0,
                rrn_credit: "000663782985",
                rrn_debit: "000663782982",
                sender_bonus: 0.0,
                sender_card_bank: "pb",
                sender_card_country: "804",
                sender_card_mask2: "473118*55",
                sender_card_type: "visa",
                sender_commission: 0.0,
                sender_first_name: "first name",
                sender_last_name: "last name",
                sender_phone: "380950000001",
                status: :success,
                transaction_id: 165_193,
                type: "buy",
                version: 3
              }} ==
               LiqPayAPI.Confirmation.cvv(
                 %LiqPayAPI.Confirmation.CVV.Request{
                   action: :confirm,
                   card_cvv: "123",
                   confirm_token: "JmV2qDgC3LotB8njGDcEYjHFpmpAZR",
                   public_key: "string",
                   version: 3
                 },
                 private_key: "a4825234f4bae72a0be04eafe9e8e2bada209255",
                 base_url: "https://example.com"
               )
    end
  end
end
