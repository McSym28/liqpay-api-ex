defmodule LiqPayAPI.TokensTest do
  use ExUnit.Case, async: true
  import Mox

  @httpoison OpenAPIClient.HTTPoisonMock
  @client OpenAPIClientMock

  setup :verify_on_exit!

  describe "obtain/2" do
    test "[200] performs a request, encodes Obtain.Request from request's body and decodes Obtain.Response from response's body" do
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
                  "action" => "token_create",
                  "card" => "string",
                  "card_cvv" => "string",
                  "card_exp_month" => "08",
                  "card_exp_year" => "19",
                  "customer" => "string",
                  "is_credit" => true,
                  "is_debit" => true,
                  "public_key" => "string",
                  "pushAccountReceipt" => "string",
                  "pushdata" => "string",
                  "version" => 3
                }} == Jason.decode(body)

        assert {:ok, body_encoded} =
                 Jason.encode(%{
                   "card_token" => "4F27B5BAA01C1FEC8E695855A53329B6EFCDF176",
                   "card_token_info" => %{
                     "decision" => "APPROVED",
                     "status" => "ACTIVE",
                     "tokenExpDate" => "0822",
                     "tokenRef" => "DM4MMC0000129713c08c30d279db433584d5b7b0d406c654",
                     "tokenSuffix" => "1234"
                   },
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
              %LiqPayAPI.Tokens.Obtain.Response{
                card_token: "4F27B5BAA01C1FEC8E695855A53329B6EFCDF176",
                card_token_info: %LiqPayAPI.Tokens.Obtain.Response.CardTokenInfo{
                  decision: :approved,
                  status: :active,
                  token_exp_date: ~D[2022-08-01],
                  token_ref: "DM4MMC0000129713c08c30d279db433584d5b7b0d406c654",
                  token_suffix: "1234"
                },
                result: :ok,
                status: "success"
              }} ==
               LiqPayAPI.Tokens.obtain(
                 %LiqPayAPI.Tokens.Obtain.Request{
                   action: :token_create,
                   card_tokenization: %LiqPayAPI.Tokens.Obtain.Request.CardTokenization{
                     card: "string",
                     card_cvv: "string",
                     card_exp_month: "08",
                     card_exp_year: "19"
                   },
                   connect_control_tokenization:
                     %LiqPayAPI.Tokens.Obtain.Request.ConnectControlTokenization{
                       push_account_receipt: "string"
                     },
                   is_credit: true,
                   is_debit: true,
                   public_key: "string",
                   vceh_tokenization: %LiqPayAPI.Tokens.Obtain.Request.VCEHTokenization{
                     customer: "string",
                     pushdata: "string"
                   },
                   version: 3
                 },
                 private_key: "a4825234f4bae72a0be04eafe9e8e2bada209255",
                 base_url: "https://example.com"
               )
    end
  end

  describe "change_status/2" do
    test "[200] performs a request, encodes ChangeStatus.Request from request's body and decodes ChangeStatus.Response from response's body" do
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
                  "action" => "token_update",
                  "card_token" => "string",
                  "card_token_action" => "DELETE",
                  "public_key" => "string",
                  "version" => 3
                }} == Jason.decode(body)

        assert {:ok, body_encoded} =
                 Jason.encode(%{
                   "card_token" => "string",
                   "card_token_info" => %{
                     "status" => "ACTIVE",
                     "tokenExpDate" => "0124",
                     "tokenRef" => "string",
                     "tokenSuffix" => "string"
                   },
                   "status" => "string"
                 })

        {:ok,
         %HTTPoison.Response{
           status_code: 200,
           headers: [{"Content-Type", "application/json"}],
           body: body_encoded
         }}
      end)

      assert {:ok,
              %LiqPayAPI.Tokens.ChangeStatus.Response{
                card_token: "string",
                card_token_info: %LiqPayAPI.Tokens.ChangeStatus.Response.CardTokenInfo{
                  status: :active,
                  token_exp_date: ~D[2024-01-01],
                  token_ref: "string",
                  token_suffix: "string"
                },
                status: "string"
              }} ==
               LiqPayAPI.Tokens.change_status(
                 %LiqPayAPI.Tokens.ChangeStatus.Request{
                   action: :token_update,
                   card_token: "string",
                   card_token_action: :delete,
                   public_key: "string",
                   version: 3
                 },
                 private_key: "a4825234f4bae72a0be04eafe9e8e2bada209255",
                 base_url: "https://example.com"
               )
    end
  end
end
