defmodule LiqPayAPI.InternetAcquiring.GPayTest do
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
                  "action" => "pay",
                  "amount" => 5.0,
                  "currency" => "EUR",
                  "description" => "string",
                  "gpay_token" => "string",
                  "language" => "uk",
                  "order_id" => "string",
                  "paytype" => "gpay",
                  "public_key" => "string",
                  "result_url" => "http://example.com",
                  "server_url" => "http://example.com",
                  "split_tickets_only" => true,
                  "version" => 3
                }} == Jason.decode(body)

        assert {:ok, body_encoded} =
                 Jason.encode(%{
                   "confirm_phone" => "380680375936",
                   "language" => "uk",
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
              %LiqPayAPI.InternetAcquiring.GPay.DecryptedToken.Response{
                confirm_phone: "380680375936",
                language: "uk",
                result: :ok
              }} ==
               LiqPayAPI.InternetAcquiring.GPay.decrypted_token(
                 %LiqPayAPI.InternetAcquiring.GPay.DecryptedToken.Request{
                   action: :pay,
                   amount: 5.0,
                   currency: :eur,
                   description: "string",
                   gpay_token: "string",
                   language: :uk,
                   order_id: "string",
                   paytype: :gpay,
                   public_key: "string",
                   result_url: "http://example.com",
                   server_url: "http://example.com",
                   split_tickets_only: true,
                   version: 3
                 },
                 private_key: "a4825234f4bae72a0be04eafe9e8e2bada209255",
                 base_url: "https://example.com"
               )
    end
  end
end
