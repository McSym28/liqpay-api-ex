defmodule LiqPayAPI.InformationTest do
  use ExUnit.Case, async: true
  import Mox

  @httpoison OpenAPIClient.HTTPoisonMock
  @client OpenAPIClientMock

  setup :verify_on_exit!

  describe "receive_receipt/2" do
    test "[200] performs a request, encodes ReceiveReceipt.Request from request's body and decodes ReceiveReceipt.Response from response's body" do
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
                  "action" => "ticket",
                  "email" => "email@gmail.com",
                  "language" => "uk",
                  "order_id" => "order_id_1",
                  "payment_id" => 1,
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
                   payment_id: 1,
                   public_key: "string",
                   version: 3
                 },
                 private_key: "a4825234f4bae72a0be04eafe9e8e2bada209255",
                 base_url: "https://example.com"
               )
    end
  end

  describe "payment_archive/2" do
    test "[200] performs a request, encodes PaymentArchive.Request from request's body and decodes PaymentArchive.Response from response's body" do
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
                       "authcode_debit" => "871225",
                       "transaction_id" => 13_289_624,
                       "amount_bonus" => 0.02,
                       "status" => "success",
                       "authcode_credit" => "887245",
                       "type" => "buy",
                       "sender_card_mask2" => "545708*24",
                       "ip" => "string",
                       "is_3ds" => true,
                       "currency" => "UAH",
                       "amount_credit" => 0.1,
                       "action" => "pay",
                       "bonus_procent" => 15.0,
                       "create_date" => 1_715_064_068_394,
                       "payment_id" => 13_289_624,
                       "version" => 3,
                       "public_key" => "i16202663459",
                       "currency_credit" => "UAH",
                       "commission_debit" => 0.0,
                       "sender_bonus" => 0.02,
                       "sender_card_country" => 804,
                       "rrn_debit" => "000000451627",
                       "amount_debit" => 0.1,
                       "sender_phone" => "380933454182",
                       "confirm_phone" => "380933454182",
                       "amount" => 0.1,
                       "commission_credit" => 0.0,
                       "currency_debit" => "UAH",
                       "sender_card_bank" => "pb",
                       "end_date" => 1_715_263_626_605,
                       "receiver_commission" => 0.0,
                       "acq_id" => 414_963,
                       "order_id" => "idOrder_ByToken_82349",
                       "card_token" => "AD002516D2E36F022951C401889E523B33DB04C7",
                       "description" => "description text check tocken",
                       "bonus_type" => "promo",
                       "agent_commission" => 0.0,
                       "sender_first_name" => "Тетяна",
                       "sender_last_name" => "Станько",
                       "mpi_eci" => 7,
                       "liqpay_order_id" => "DPARG4EF1715064068384258",
                       "sender_card_type" => "mc",
                       "paytype" => "token",
                       "rrn_credit" => "000000454083",
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
                    action: :pay,
                    agent_commission: 0.0,
                    amount: 0.1,
                    amount_bonus: 0.02,
                    amount_credit: 0.1,
                    amount_debit: 0.1,
                    authcode_credit: "887245",
                    authcode_debit: "871225",
                    bonus_procent: 15.0,
                    bonus_type: :promo,
                    card_token: "AD002516D2E36F022951C401889E523B33DB04C7",
                    commission_credit: 0.0,
                    commission_debit: 0.0,
                    confirm_phone: "380933454182",
                    create_date: ~U[2024-05-07 06:41:08.394Z],
                    currency: "UAH",
                    currency_credit: "UAH",
                    currency_debit: "UAH",
                    description: "description text check tocken",
                    end_date: ~U[2024-05-09 14:07:06.605Z],
                    ip: "string",
                    is_3ds: true,
                    liqpay_order_id: "DPARG4EF1715064068384258",
                    mpi_eci: 7,
                    order_id: "idOrder_ByToken_82349",
                    payment_id: 13_289_624,
                    paytype: "token",
                    public_key: "i16202663459",
                    receiver_commission: 0.0,
                    rrn_credit: "000000454083",
                    rrn_debit: "000000451627",
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
                    transaction_id: 13_289_624,
                    type: "buy",
                    version: 3
                  }
                ],
                result: "success"
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
                 private_key: "a4825234f4bae72a0be04eafe9e8e2bada209255",
                 base_url: "https://example.com"
               )
    end
  end

  describe "status_payment/2" do
    test "[200] performs a request, encodes StatusPayment.Request from request's body and decodes StatusPayment.Response from response's body" do
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
                   "amount" => 0.1,
                   "amount_bonus" => 0.04,
                   "amount_credit" => 0.1,
                   "amount_debit" => 0.1,
                   "authcode_credit" => "056268",
                   "authcode_debit" => "692267",
                   "bonus_procent" => 40.0,
                   "bonus_type" => "promo",
                   "card_token" => "01D6145196B7642140BB9BCCBBB006CBD8A2AEB3",
                   "commission_credit" => 0.0,
                   "commission_debit" => 0.0,
                   "confirm_phone" => "380933454182",
                   "create_date" => 1_715_597_977_414,
                   "currency" => "UAH",
                   "currency_credit" => "UAH",
                   "currency_debit" => "UAH",
                   "description" => "testing pay by card",
                   "end_date" => 1_715_598_023_416,
                   "info" => "string",
                   "ip" => "string",
                   "is_3ds" => true,
                   "language" => "uk",
                   "liqpay_order_id" => "A3DP00HW1715598022008080",
                   "moment_part" => true,
                   "mpi_eci" => 7,
                   "order_id" => "idByCard34в5D30eсм7x",
                   "payment_id" => 13_291_813,
                   "paytype" => "card",
                   "public_key" => "i16202663459",
                   "receiver_commission" => 0.0,
                   "result" => "ok",
                   "rrn_credit" => "000000456861",
                   "rrn_debit" => "000000456859",
                   "sender_bonus" => 0.04,
                   "sender_card_bank" => "pb",
                   "sender_card_country" => 804,
                   "sender_card_mask2" => "545708*24",
                   "sender_card_type" => "mc",
                   "sender_commission" => 0.0,
                   "sender_first_name" => "First name",
                   "sender_last_name" => "Last name",
                   "sender_phone" => "380933454182",
                   "status" => "success",
                   "transaction_id" => 13_291_813,
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
                amount: 0.1,
                amount_bonus: 0.04,
                amount_credit: 0.1,
                amount_debit: 0.1,
                authcode_credit: "056268",
                authcode_debit: "692267",
                bonus_procent: 40.0,
                bonus_type: :promo,
                card_token: "01D6145196B7642140BB9BCCBBB006CBD8A2AEB3",
                commission_credit: 0.0,
                commission_debit: 0.0,
                confirm_phone: "380933454182",
                create_date: ~U[2024-05-13 10:59:37.414Z],
                currency: "UAH",
                currency_credit: "UAH",
                currency_debit: "UAH",
                description: "testing pay by card",
                end_date: ~U[2024-05-13 11:00:23.416Z],
                info: "string",
                ip: "string",
                is_3ds: true,
                language: "uk",
                liqpay_order_id: "A3DP00HW1715598022008080",
                moment_part: true,
                mpi_eci: 7,
                order_id: "idByCard34в5D30eсм7x",
                payment_id: 13_291_813,
                paytype: :card,
                public_key: "i16202663459",
                receiver_commission: 0.0,
                result: :ok,
                rrn_credit: "000000456861",
                rrn_debit: "000000456859",
                sender_bonus: 0.04,
                sender_card_bank: "pb",
                sender_card_country: 804,
                sender_card_mask2: "545708*24",
                sender_card_type: "mc",
                sender_commission: 0.0,
                sender_first_name: "First name",
                sender_last_name: "Last name",
                sender_phone: "380933454182",
                status: :success,
                transaction_id: 13_291_813,
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
                 private_key: "a4825234f4bae72a0be04eafe9e8e2bada209255",
                 base_url: "https://example.com"
               )
    end
  end

  describe "adding_data/2" do
    test "[200] performs a request, encodes AddingData.Request from request's body and decodes AddingData.Response from response's body" do
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
                   "amount" => 1.0,
                   "amount_bonus" => 5.98,
                   "amount_credit" => 39.84,
                   "amount_debit" => 39.84,
                   "authcode_credit" => "058250",
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
                   "description" => "test_pay_googlePay",
                   "end_date" => 1_715_263_629_970,
                   "info" => "new info adding for test",
                   "ip" => "8.8.8.8",
                   "is_3ds" => true,
                   "language" => "uk",
                   "liqpay_order_id" => "JH1W3RSE1715080189403270",
                   "moment_part" => true,
                   "mpi_eci" => 7,
                   "order_id" => "id_gp7yxus776eг",
                   "payment_id" => 13_289_654,
                   "paytype" => "token",
                   "public_key" => "i16202663459",
                   "receiver_commission" => 0.0,
                   "result" => "ok",
                   "rrn_credit" => "000000454090",
                   "rrn_debit" => "000000451646",
                   "sender_bonus" => 0.15,
                   "sender_card_bank" => "pb",
                   "sender_card_country" => 804,
                   "sender_card_mask2" => "545708*24",
                   "sender_card_type" => "mc",
                   "sender_commission" => 0.0,
                   "sender_first_name" => "Тетяна",
                   "sender_last_name" => "Станько",
                   "sender_phone" => "380933454182",
                   "status" => "success",
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
              %LiqPayAPI.Information.AddingData.Response{
                acq_id: 414_963,
                action: :pay,
                agent_commission: 0.0,
                amount: 1.0,
                amount_bonus: 5.98,
                amount_credit: 39.84,
                amount_debit: 39.84,
                authcode_credit: "058250",
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
                description: "test_pay_googlePay",
                end_date: ~U[2024-05-09 14:07:09.970Z],
                info: "new info adding for test",
                ip: "8.8.8.8",
                is_3ds: true,
                language: "uk",
                liqpay_order_id: "JH1W3RSE1715080189403270",
                moment_part: true,
                mpi_eci: 7,
                order_id: "id_gp7yxus776eг",
                payment_id: 13_289_654,
                paytype: "token",
                public_key: "i16202663459",
                receiver_commission: 0.0,
                result: :ok,
                rrn_credit: "000000454090",
                rrn_debit: "000000451646",
                sender_bonus: 0.15,
                sender_card_bank: "pb",
                sender_card_country: 804,
                sender_card_mask2: "545708*24",
                sender_card_type: "mc",
                sender_commission: 0.0,
                sender_first_name: "Тетяна",
                sender_last_name: "Станько",
                sender_phone: "380933454182",
                status: :success,
                transaction_id: 13_289_654,
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
                 private_key: "a4825234f4bae72a0be04eafe9e8e2bada209255",
                 base_url: "https://example.com"
               )
    end
  end
end
