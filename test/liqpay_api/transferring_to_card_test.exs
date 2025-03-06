defmodule LiqPayAPI.TransferringToCardTest do
  use ExUnit.Case, async: true
  import Mox

  @httpoison OpenAPIClient.HTTPoisonMock
  @client OpenAPIClientMock

  setup :verify_on_exit!

  describe "transfer/2" do
    test "[200] performs a request, encodes Transfer.Request from request's body and decodes Transfer.Response from response's body" do
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
                  "action" => "p2pcredit",
                  "amount" => 5.0,
                  "currency" => "USD",
                  "customer" => "string",
                  "description" => "description text",
                  "info" => "External information for payments",
                  "ip" => "string",
                  "language" => "uk",
                  "order_id" => "order_id_1",
                  "public_key" => "string",
                  "receiver_account" => %{
                    "receiver_account" => "string",
                    "receiver_company" => "string",
                    "receiver_mfo" => "string",
                    "receiver_okpo" => "string"
                  },
                  "receiver_card" => "4731195301524633",
                  "receiver_card_token" => "B5BВB0D00B88B00ED00A00D0D",
                  "receiver_first_name" => "FirstName",
                  "receiver_last_name" => "LastName",
                  "sender" => %{
                    "sender_address" => "string",
                    "sender_city" => "string",
                    "sender_country_code" => "string",
                    "sender_first_name" => "string",
                    "sender_last_name" => "string",
                    "sender_postal_code" => "string"
                  },
                  "server_url" => "http://example.com",
                  "taxed" => "Income is not subject to tax",
                  "version" => 3
                }} == Jason.decode(body)

        assert {:ok, body_encoded} =
                 Jason.encode(%{
                   "acq_id" => 414_963,
                   "action" => "p2pcredit",
                   "agent_commission" => 0.0,
                   "amount" => 0.1,
                   "amount_bonus" => 0.0,
                   "amount_credit" => 0.1,
                   "amount_debit" => 0.1,
                   "commission_credit" => 0.0,
                   "commission_debit" => 0.0,
                   "create_date" => 1_501_687_336_377,
                   "currency" => "UAH",
                   "currency_credit" => "UAH",
                   "currency_debit" => "UAH",
                   "description" => "description text",
                   "end_date" => 1_501_687_336_377,
                   "is_3ds" => true,
                   "liqpay_order_id" => "NYMK3AE61501685438251925",
                   "mpi_eci" => 7,
                   "order_id" => "98R1U1OV1485849059893399",
                   "payment_id" => 165_189,
                   "public_key" => "i000000000",
                   "receiver_commission" => 0.0,
                   "redirect_to" => "string",
                   "sender_bonus" => 0.0,
                   "sender_commission" => 0.0,
                   "status" => "success",
                   "transaction_id" => 165_189,
                   "type" => "p2pcredit",
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
              %LiqPayAPI.TransferringToCard.Transfer.Response{
                acq_id: 414_963,
                action: "p2pcredit",
                agent_commission: 0.0,
                amount: 0.1,
                amount_bonus: 0.0,
                amount_credit: 0.1,
                amount_debit: 0.1,
                commission_credit: 0.0,
                commission_debit: 0.0,
                create_date: ~U[2017-08-02 15:22:16.377Z],
                currency: "UAH",
                currency_credit: "UAH",
                currency_debit: "UAH",
                description: "description text",
                end_date: ~U[2017-08-02 15:22:16.377Z],
                is_3ds: true,
                liqpay_order_id: "NYMK3AE61501685438251925",
                mpi_eci: 7,
                order_id: "98R1U1OV1485849059893399",
                payment_id: 165_189,
                public_key: "i000000000",
                receiver_commission: 0.0,
                redirect_to: "string",
                sender_bonus: 0.0,
                sender_commission: 0.0,
                status: :success,
                transaction_id: 165_189,
                type: "p2pcredit",
                version: 3
              }} ==
               LiqPayAPI.TransferringToCard.transfer(
                 %LiqPayAPI.TransferringToCard.Transfer.Request{
                   action: :p2pcredit,
                   amount: 5.0,
                   currency: :usd,
                   customer: "string",
                   description: "description text",
                   info: "External information for payments",
                   ip: "string",
                   language: :uk,
                   order_id: "order_id_1",
                   public_key: "string",
                   receiver_account:
                     %LiqPayAPI.TransferringToCard.Transfer.Request.ReceiverAccount{
                       receiver_account: "string",
                       receiver_company: "string",
                       receiver_mfo: "string",
                       receiver_okpo: "string"
                     },
                   receiver_card: "4731195301524633",
                   receiver_card_token: "B5BВB0D00B88B00ED00A00D0D",
                   receiver_first_name: "FirstName",
                   receiver_last_name: "LastName",
                   sender: %LiqPayAPI.TransferringToCard.Transfer.Request.Sender{
                     sender_address: "string",
                     sender_city: "string",
                     sender_country_code: "string",
                     sender_first_name: "string",
                     sender_last_name: "string",
                     sender_postal_code: "string"
                   },
                   server_url: "http://example.com",
                   taxed: :"income is not subject to tax",
                   version: 3
                 },
                 private_key: "a4825234f4bae72a0be04eafe9e8e2bada209255",
                 base_url: "https://example.com"
               )
    end
  end
end
