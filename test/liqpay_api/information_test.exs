defmodule LiqPayAPI.InformationTest do
  use ExUnit.Case, async: true
  import Mox

  @httpoison OpenAPIClient.HTTPoisonMock
  @client OpenAPIClientMock

  setup :verify_on_exit!

  describe "receive_receipt/2" do
    test "[200] performs a request, encodes ReceiveReceipt.Request from request's body and decodes ReceiveReceipt.Response from response's body" do
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
                  "action" => "ticket",
                  "email" => "email@gmail.com",
                  "language" => "uk",
                  "order_id" => "order_id_1",
                  "payment_id" => 1.0,
                  "public_key" => "string",
                  "version" => 3
                }} == Jason.decode(body)

        assert {:ok, body_encoded} = Jason.encode(%{"result" => "ok"})

        {:ok,
         %HTTPoison.Response{
           status_code: 200,
           headers: [{"Content-Type", "application/json"}],
           body: body_encoded
         }}
      end)

      assert {:ok, %LiqPayAPI.Information.ReceiveReceipt.Response{result: :ok}} ==
               LiqPayAPI.Information.receive_receipt(
                 %LiqPayAPI.Information.ReceiveReceipt.Request{
                   action: :ticket,
                   email: "email@gmail.com",
                   language: :uk,
                   order_id: "order_id_1",
                   payment_id: 1.0,
                   public_key: "string",
                   version: 3
                 },
                 base_url: "https://example.com"
               )
    end
  end

  describe "payment_archive/2" do
    test "[200] performs a request, encodes PaymentArchive.Request from request's body and decodes PaymentArchive.Response from response's body" do
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
                  "action" => "reports",
                  "date_from" => 1_443_161_386_000,
                  "date_to" => 1_443_164_386_000,
                  "public_key" => "string",
                  "resp_format" => "json",
                  "version" => 3
                }} == Jason.decode(body)

        assert {:ok, body_encoded} =
                 Jason.encode(%{
                   "data" => [
                     %{
                       "authcode_debit" => "557642",
                       "transaction_id" => 157_018,
                       "amount_bonus" => 0.0,
                       "status" => "success",
                       "authcode_credit" => "790643",
                       "type" => "regular",
                       "sender_card_mask2" => "414962*99",
                       "is_3ds" => true,
                       "currency" => "UAH",
                       "amount_credit" => 0.02,
                       "action" => "regular",
                       "create_date" => 1_498_226_196_229,
                       "payment_id" => 157_018,
                       "version" => 3,
                       "public_key" => "i000000000",
                       "currency_credit" => "UAH",
                       "commission_debit" => 0.0,
                       "sender_bonus" => 0.0,
                       "sender_card_country" => 804,
                       "rrn_debit" => "000637410774",
                       "amount_debit" => 0.02,
                       "sender_phone" => "380950000001",
                       "amount" => 0.02,
                       "commission_credit" => 0.0,
                       "currency_debit" => "UAH",
                       "sender_card_bank" => "pb",
                       "end_date" => 1_498_226_199_886,
                       "receiver_commission" => 0.0,
                       "acq_id" => 414_963,
                       "order_id" => "98R1U1OV1485849059893399",
                       "description" => "test",
                       "agent_commission" => 0.0,
                       "sender_first_name" => "test",
                       "sender_last_name" => "test",
                       "mpi_eci" => 7,
                       "liqpay_order_id" => "NYMK3AE61501685438251925",
                       "sender_card_type" => "visa",
                       "paytype" => "card",
                       "rrn_credit" => "000637410802",
                       "sender_commission" => 0.0
                     }
                   ],
                   "result" => "success"
                 })

        {:ok,
         %HTTPoison.Response{
           status_code: 200,
           headers: [{"Content-Type", "application/json"}],
           body: body_encoded
         }}
      end)

      assert {:ok,
              %LiqPayAPI.Information.PaymentArchive.Response{
                data: [
                  %LiqPayAPI.Information.PaymentArchive.Response.Data{
                    acq_id: 414_963,
                    action: :regular,
                    agent_commission: 0.0,
                    amount: 0.02,
                    amount_bonus: 0.0,
                    amount_credit: 0.02,
                    amount_debit: 0.02,
                    authcode_credit: "790643",
                    authcode_debit: "557642",
                    commission_credit: 0.0,
                    commission_debit: 0.0,
                    create_date: ~U[2017-06-23 13:56:36.229Z],
                    currency: "UAH",
                    currency_credit: "UAH",
                    currency_debit: "UAH",
                    description: "test",
                    end_date: ~U[2017-06-23 13:56:39.886Z],
                    is_3ds: true,
                    liqpay_order_id: "NYMK3AE61501685438251925",
                    mpi_eci: 7,
                    order_id: "98R1U1OV1485849059893399",
                    payment_id: 157_018,
                    paytype: :card,
                    public_key: "i000000000",
                    receiver_commission: 0.0,
                    rrn_credit: "000637410802",
                    rrn_debit: "000637410774",
                    sender_bonus: 0.0,
                    sender_card_bank: "pb",
                    sender_card_country: "804",
                    sender_card_mask2: "414962*99",
                    sender_card_type: "visa",
                    sender_commission: 0.0,
                    sender_first_name: "test",
                    sender_last_name: "test",
                    sender_phone: "380950000001",
                    status: :success,
                    transaction_id: 157_018,
                    type: "regular",
                    version: 3
                  }
                ],
                result: :success
              }} ==
               LiqPayAPI.Information.payment_archive(
                 %LiqPayAPI.Information.PaymentArchive.Request{
                   action: :reports,
                   date_from: ~U[2015-09-25 06:09:46.000Z],
                   date_to: ~U[2015-09-25 06:59:46.000Z],
                   public_key: "string",
                   resp_format: :json,
                   version: 3
                 },
                 base_url: "https://example.com"
               )
    end
  end

  describe "status_payment/2" do
    test "[200] performs a request, encodes StatusPayment.Request from request's body and decodes StatusPayment.Response from response's body" do
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
                  "action" => "status",
                  "order_id" => "order_id_1",
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
                   "authcode_credit" => "703006",
                   "authcode_debit" => "108527",
                   "bonus_procent" => 7.0,
                   "bonus_type" => "bonusplus",
                   "card_token" => "2DFBFE626B7341611450DE81E971E948D6F260",
                   "commission_credit" => 0.0,
                   "commission_debit" => 0.0,
                   "create_date" => 1_501_757_716_373,
                   "currency" => "UAH",
                   "currency_credit" => "UAH",
                   "currency_debit" => "UAH",
                   "description" => "test",
                   "end_date" => 1_501_757_729_972,
                   "info" => "My information",
                   "ip" => "8.8.8.8",
                   "is_3ds" => true,
                   "liqpay_order_id" => "NYMK3AE61501685438251925",
                   "moment_part" => true,
                   "mpi_eci" => 7,
                   "order_id" => "98R1U1OV1485849059893399",
                   "payment_id" => 165_629,
                   "paytype" => "card",
                   "public_key" => "i000000000",
                   "receiver_commission" => 0.0,
                   "rrn_credit" => "000664267607",
                   "rrn_debit" => "000664267598",
                   "sender_bonus" => 0.0,
                   "sender_card_bank" => "pb",
                   "sender_card_country" => 804,
                   "sender_card_mask2" => "473118*97",
                   "sender_card_type" => "visa",
                   "sender_commission" => 0.0,
                   "sender_phone" => "380950000001",
                   "status" => "success",
                   "transaction_id" => 165_629,
                   "type" => "buy",
                   "version" => 3,
                   "wait_reserve_status" => true
                 })

        {:ok,
         %HTTPoison.Response{
           status_code: 200,
           headers: [{"Content-Type", "application/json"}],
           body: body_encoded
         }}
      end)

      assert {:ok,
              %LiqPayAPI.Information.StatusPayment.Response{
                acq_id: 414_963,
                action: :pay,
                agent_commission: 0.0,
                amount: 0.02,
                amount_bonus: 0.0,
                amount_credit: 0.02,
                amount_debit: 0.02,
                authcode_credit: "703006",
                authcode_debit: "108527",
                bonus_procent: 7.0,
                bonus_type: :bonusplus,
                card_token: "2DFBFE626B7341611450DE81E971E948D6F260",
                commission_credit: 0.0,
                commission_debit: 0.0,
                create_date: ~U[2017-08-03 10:55:16.373Z],
                currency: "UAH",
                currency_credit: "UAH",
                currency_debit: "UAH",
                description: "test",
                end_date: ~U[2017-08-03 10:55:29.972Z],
                info: "My information",
                ip: "8.8.8.8",
                is_3ds: true,
                liqpay_order_id: "NYMK3AE61501685438251925",
                moment_part: true,
                mpi_eci: 7,
                order_id: "98R1U1OV1485849059893399",
                payment_id: 165_629,
                paytype: :card,
                public_key: "i000000000",
                receiver_commission: 0.0,
                rrn_credit: "000664267607",
                rrn_debit: "000664267598",
                sender_bonus: 0.0,
                sender_card_bank: "pb",
                sender_card_country: "804",
                sender_card_mask2: "473118*97",
                sender_card_type: "visa",
                sender_commission: 0.0,
                sender_phone: "380950000001",
                status: :success,
                transaction_id: 165_629,
                type: "buy",
                version: 3,
                wait_reserve_status: true
              }} ==
               LiqPayAPI.Information.status_payment(
                 %LiqPayAPI.Information.StatusPayment.Request{
                   action: :status,
                   order_id: "order_id_1",
                   public_key: "string",
                   version: 3
                 },
                 base_url: "https://example.com"
               )
    end
  end

  describe "adding_data/2" do
    test "[200] performs a request, encodes AddingData.Request from request's body and decodes AddingData.Response from response's body" do
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
                  "action" => "data",
                  "info" => "External information for payments",
                  "order_id" => "order_id_1",
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
                   "authcode_credit" => "703006",
                   "authcode_debit" => "108527",
                   "bonus_procent" => 7.0,
                   "bonus_type" => "bonusplus",
                   "card_token" => "2DFBFE626B7341611450DE81E971E948D6F260",
                   "commission_credit" => 0.0,
                   "commission_debit" => 0.0,
                   "create_date" => 1_501_757_716_373,
                   "currency" => "UAH",
                   "currency_credit" => "UAH",
                   "currency_debit" => "UAH",
                   "description" => "test",
                   "end_date" => 1_501_757_729_972,
                   "info" => "My information",
                   "ip" => "8.8.8.8",
                   "is_3ds" => true,
                   "liqpay_order_id" => "NYMK3AE61501685438251925",
                   "moment_part" => true,
                   "mpi_eci" => 7,
                   "order_id" => "98R1U1OV1485849059893399",
                   "payment_id" => 165_629,
                   "paytype" => "card",
                   "public_key" => "i000000000",
                   "receiver_commission" => 0.0,
                   "rrn_credit" => "000664267607",
                   "rrn_debit" => "000664267598",
                   "sender_bonus" => 0.0,
                   "sender_card_bank" => "pb",
                   "sender_card_country" => 804,
                   "sender_card_mask2" => "473118*97",
                   "sender_card_type" => "visa",
                   "sender_commission" => 0.0,
                   "sender_phone" => "380950000001",
                   "status" => "success",
                   "transaction_id" => 165_629,
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
              %LiqPayAPI.Information.AddingData.Response{
                acq_id: 414_963,
                action: :pay,
                agent_commission: 0.0,
                amount: 0.02,
                amount_bonus: 0.0,
                amount_credit: 0.02,
                amount_debit: 0.02,
                authcode_credit: "703006",
                authcode_debit: "108527",
                bonus_procent: 7.0,
                bonus_type: :bonusplus,
                card_token: "2DFBFE626B7341611450DE81E971E948D6F260",
                commission_credit: 0.0,
                commission_debit: 0.0,
                create_date: ~U[2017-08-03 10:55:16.373Z],
                currency: "UAH",
                currency_credit: "UAH",
                currency_debit: "UAH",
                description: "test",
                end_date: ~U[2017-08-03 10:55:29.972Z],
                info: "My information",
                ip: "8.8.8.8",
                is_3ds: true,
                liqpay_order_id: "NYMK3AE61501685438251925",
                moment_part: true,
                mpi_eci: 7,
                order_id: "98R1U1OV1485849059893399",
                payment_id: 165_629,
                paytype: :card,
                public_key: "i000000000",
                receiver_commission: 0.0,
                rrn_credit: "000664267607",
                rrn_debit: "000664267598",
                sender_bonus: 0.0,
                sender_card_bank: "pb",
                sender_card_country: "804",
                sender_card_mask2: "473118*97",
                sender_card_type: "visa",
                sender_commission: 0.0,
                sender_phone: "380950000001",
                status: :success,
                transaction_id: 165_629,
                type: "buy",
                version: 3
              }} ==
               LiqPayAPI.Information.adding_data(
                 %LiqPayAPI.Information.AddingData.Request{
                   action: :data,
                   info: "External information for payments",
                   order_id: "order_id_1",
                   public_key: "string",
                   version: 3
                 },
                 base_url: "https://example.com"
               )
    end
  end
end
