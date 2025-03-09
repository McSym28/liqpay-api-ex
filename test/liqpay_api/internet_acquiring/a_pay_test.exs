defmodule LiqPayAPI.InternetAcquiring.APayTest do
  use ExUnit.Case, async: true
  import Mox

  @httpoison OpenAPIClient.HTTPoisonMock
  @client OpenAPIClientMock

  setup :verify_on_exit!

  describe "decrypted_token/2" do
    test "[200] performs a request, encodes DecryptedToken.Request from request's body and decodes DecryptedToken.Response from response's body" do
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
                  "applepay_token" => "string",
                  "currency" => "EUR",
                  "dae" => "string",
                  "description" => "string",
                  "info" => "External information for payments",
                  "language" => "uk",
                  "order_id" => "string",
                  "paytype" => "apay",
                  "phone" => "+380950000001",
                  "product_category" => "string",
                  "product_description" => "string",
                  "product_name" => "string",
                  "product_url" => "http://example.com",
                  "public_key" => "string",
                  "result_url" => "http://example.com",
                  "sender_address" => "string",
                  "sender_city" => "string",
                  "sender_country_code" => "string",
                  "sender_first_name" => "string",
                  "sender_last_name" => "string",
                  "sender_postal_code" => "string",
                  "server_url" => "http://example.com",
                  "split_rules" =>
                    "[{\"amount\":404,\"commission_payer\":\"sender\",\"description\":\"string\",\"public_key\":\"i000000001\",\"rro_info\":{\"delivery_emails\":[\"string\"],\"items\":[{\"amount\":2,\"cost\":404,\"id\":123456,\"price\":202}]},\"server_url\":\"https://server1/callback\"}]",
                  "split_tickets_only" => true,
                  "version" => 3
                }} == Jason.decode(body)

        assert {:ok, body_encoded} =
                 Jason.encode(%{
                   "authcode_debit" => "037023",
                   "language" => "uk",
                   "public_key" => "i79069940442",
                   "result" => "ok"
                 })

        {:ok,
         %HTTPoison.Response{
           status_code: 200,
           headers: [{"Content-Type", "application/json"}],
           body: body_encoded
         }}
      end)

      assert {:ok,
              %LiqPayAPI.InternetAcquiring.APay.DecryptedToken.Response{
                authcode_debit: "037023",
                language: "uk",
                public_key: "i79069940442",
                result: :ok
              }} ==
               LiqPayAPI.InternetAcquiring.APay.decrypted_token(
                 %LiqPayAPI.InternetAcquiring.APay.DecryptedToken.Request{
                   action: :hold,
                   amount: 5.0,
                   applepay_token: "string",
                   currency: :eur,
                   dae: "string",
                   description: "string",
                   info: "External information for payments",
                   language: :uk,
                   order_id: "string",
                   paytype: :apay,
                   phone: "+380950000001",
                   product_category: "string",
                   product_description: "string",
                   product_name: "string",
                   product_url: "http://example.com",
                   public_key: "string",
                   result_url: "http://example.com",
                   sender: %LiqPayAPI.InternetAcquiring.APay.DecryptedToken.Request.Sender{
                     sender_address: "string",
                     sender_city: "string",
                     sender_country_code: "string",
                     sender_first_name: "string",
                     sender_last_name: "string",
                     sender_postal_code: "string"
                   },
                   server_url: "http://example.com",
                   split_rules: [
                     %LiqPayAPI.InternetAcquiring.APay.DecryptedToken.Request.SplitRules{
                       amount: 404,
                       commission_payer: :sender,
                       description: "string",
                       public_key: "i000000001",
                       rro_info:
                         %LiqPayAPI.InternetAcquiring.APay.DecryptedToken.Request.SplitRulesRroInfo{
                           delivery_emails: ["string"],
                           items: [
                             %LiqPayAPI.InternetAcquiring.APay.DecryptedToken.Request.SplitRulesRroInfoItems{
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
end
