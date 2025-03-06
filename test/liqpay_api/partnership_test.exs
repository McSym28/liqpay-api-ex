defmodule LiqPayAPI.PartnershipTest do
  use ExUnit.Case, async: true
  import Mox

  @httpoison OpenAPIClient.HTTPoisonMock
  @client OpenAPIClientMock

  setup :verify_on_exit!

  describe "shop_edit/2" do
    test "[200] performs a request, encodes ShopEdit.Request from request's body and decodes ShopEdit.Response from response's body" do
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
                  "action" => "agent_shop_edit",
                  "amount_procent_agent" => 1.0,
                  "amount_static_agent" => 1.0,
                  "can_checkout_edit" => true,
                  "can_reports" => true,
                  "company" => "string",
                  "currency_static_agent" => "string",
                  "description" => "site description",
                  "email" => "email@gmail.com",
                  "iban" => "string",
                  "logo" => "string",
                  "merchant_public_key" => "i3111000000",
                  "name" => "Site name",
                  "okpo" => "string",
                  "phone" => "+380950000001",
                  "public_key" => "string",
                  "public_phone" => "string",
                  "site" => "agent1.site.com",
                  "version" => 3
                }} == Jason.decode(body)

        assert {:ok, body_encoded} =
                 Jason.encode(%{
                   "private_key" => "wGsqoko5412LcD0vB215XK2wQSgLDVBrsaPIRi6",
                   "public_key" => "i000000000",
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
              %LiqPayAPI.Partnership.ShopEdit.Response{
                private_key: "wGsqoko5412LcD0vB215XK2wQSgLDVBrsaPIRi6",
                public_key: "i000000000",
                status: :success
              }} ==
               LiqPayAPI.Partnership.shop_edit(
                 %LiqPayAPI.Partnership.ShopEdit.Request{
                   action: :agent_shop_edit,
                   amount_procent_agent: 1.0,
                   amount_static_agent: 1.0,
                   can_checkout_edit: true,
                   can_reports: true,
                   company: "string",
                   currency_static_agent: "string",
                   description: "site description",
                   email: "email@gmail.com",
                   iban: "string",
                   logo: "string",
                   merchant_public_key: "i3111000000",
                   name: "Site name",
                   okpo: "string",
                   phone: "+380950000001",
                   public_key: "string",
                   public_phone: "string",
                   site: "agent1.site.com",
                   version: 3
                 },
                 private_key: "a4825234f4bae72a0be04eafe9e8e2bada209255",
                 base_url: "https://example.com"
               )
    end
  end

  describe "info_user/2" do
    test "[200] performs a request, encodes InfoUser.Request from request's body and decodes InfoUser.Response from response's body" do
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
                  "action" => "agent_info_user",
                  "phone" => "+380950000001",
                  "public_key" => "string",
                  "version" => 3
                }} == Jason.decode(body)

        assert {:ok, body_encoded} =
                 Jason.encode(%{
                   "data" => [
                     %{
                       "blocked" => true,
                       "create_date" => 1_491_294_687_862,
                       "description" => "site description",
                       "email" => "test@gmail.com",
                       "logo" => "string",
                       "name" => "Shop",
                       "public_key" => "i000000000",
                       "role" => "undefined",
                       "update_date" => 1_491_294_687_862,
                       "url" => "http://test.com"
                     }
                   ],
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
              %LiqPayAPI.Partnership.InfoUser.Response{
                data: [
                  %LiqPayAPI.Partnership.InfoUser.Response.Data{
                    blocked: true,
                    create_date: ~U[2017-04-04 08:31:27.862Z],
                    description: "site description",
                    email: "test@gmail.com",
                    logo: "string",
                    name: "Shop",
                    public_key: "i000000000",
                    role: "undefined",
                    update_date: ~U[2017-04-04 08:31:27.862Z],
                    url: "http://test.com"
                  }
                ],
                result: :ok
              }} ==
               LiqPayAPI.Partnership.info_user(
                 %LiqPayAPI.Partnership.InfoUser.Request{
                   action: :agent_info_user,
                   phone: "+380950000001",
                   public_key: "string",
                   version: 3
                 },
                 private_key: "a4825234f4bae72a0be04eafe9e8e2bada209255",
                 base_url: "https://example.com"
               )
    end
  end

  describe "info_merchant/2" do
    test "[200] performs a request, encodes InfoMerchant.Request from request's body and decodes InfoMerchant.Response from response's body" do
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
                  "action" => "agent_info_merchant",
                  "language" => "uk",
                  "merchant_public_key" => "i3111000000",
                  "public_key" => "string",
                  "version" => 3
                }} == Jason.decode(body)

        assert {:ok, body_encoded} =
                 Jason.encode(%{
                   "blocked" => true,
                   "comment" => "string",
                   "company_name" => "string",
                   "create_date" => 1_501_764_124_764,
                   "description" => "site description",
                   "email" => "test@gmail.com",
                   "link" => "string",
                   "logo" => "string",
                   "mfo" => "string",
                   "name" => "Site name",
                   "okpo" => "string",
                   "phone" => "380950000001",
                   "public_key" => "i000000000",
                   "refund_number" => "4731180000000001",
                   "refund_way" => "card",
                   "result" => "ok",
                   "status" => "activated",
                   "status_description" => "string",
                   "update_date" => 1_501_764_124_764,
                   "url" => "site123.site.com"
                 })

        {:ok,
         %HTTPoison.Response{
           status_code: 200,
           headers: [{"Content-Type", "application/json"}],
           body: body_encoded
         }}
      end)

      assert {:ok,
              %LiqPayAPI.Partnership.InfoMerchant.Response{
                blocked: true,
                comment: "string",
                company_name: "string",
                create_date: ~U[2017-08-03 12:42:04.764Z],
                description: "site description",
                email: "test@gmail.com",
                link: "string",
                logo: "string",
                mfo: "string",
                name: "Site name",
                okpo: "string",
                phone: "380950000001",
                public_key: "i000000000",
                refund_number: "4731180000000001",
                refund_way: "card",
                result: :ok,
                status: :activated,
                status_description: "string",
                update_date: ~U[2017-08-03 12:42:04.764Z],
                url: "site123.site.com"
              }} ==
               LiqPayAPI.Partnership.info_merchant(
                 %LiqPayAPI.Partnership.InfoMerchant.Request{
                   action: :agent_info_merchant,
                   language: :uk,
                   merchant_public_key: "i3111000000",
                   public_key: "string",
                   version: 3
                 },
                 private_key: "a4825234f4bae72a0be04eafe9e8e2bada209255",
                 base_url: "https://example.com"
               )
    end
  end
end
