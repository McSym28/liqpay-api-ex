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
                   "private_key" => "A4iOYADwxEnd0EIObBR0Plq6P1B83ko7RrjyEBFE",
                   "public_key" => "i59535170441",
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
              %LiqPayAPI.Partnership.ShopEdit.Response{
                private_key: "A4iOYADwxEnd0EIObBR0Plq6P1B83ko7RrjyEBFE",
                public_key: "i59535170441",
                result: :ok,
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
                       "create_date" => 1_706_273_095_032,
                       "description" => "site description",
                       "email" => "lost@gmail.com",
                       "logo" => "string",
                       "name" => "New name",
                       "public_key" => "i59535170441",
                       "public_phone" => "string",
                       "role" => "undefined",
                       "update_date" => 1_706_273_095_032,
                       "url" => "agent1.site.com"
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
                    create_date: ~U[2024-01-26 12:44:55.032Z],
                    description: "site description",
                    email: "lost@gmail.com",
                    logo: "string",
                    name: "New name",
                    public_key: "i59535170441",
                    public_phone: "string",
                    role: "undefined",
                    update_date: ~U[2024-01-26 12:44:55.032Z],
                    url: "agent1.site.com"
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
                   "category" => "string",
                   "comment" => "string",
                   "company_name" => "Site name",
                   "create_date" => 1_706_273_095_032,
                   "description" => "site description",
                   "email" => "lost@gmail.com",
                   "link" => "string",
                   "logo" => "string",
                   "mcc" => "8999",
                   "mfo" => "string",
                   "name" => "New name",
                   "okpo" => "string",
                   "phone" => "380933454182",
                   "public_key" => "i59535170441",
                   "public_phone" => "string",
                   "refund_number" => "string",
                   "refund_way" => "string",
                   "result" => "ok",
                   "status" => "new",
                   "status_description" => "string",
                   "update_date" => 1_706_273_095_032,
                   "url" => "agent1.site.com"
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
                category: "string",
                comment: "string",
                company_name: "Site name",
                create_date: ~U[2024-01-26 12:44:55.032Z],
                description: "site description",
                email: "lost@gmail.com",
                link: "string",
                logo: "string",
                mcc: "8999",
                mfo: "string",
                name: "New name",
                okpo: "string",
                phone: "380933454182",
                public_key: "i59535170441",
                public_phone: "string",
                refund_number: "string",
                refund_way: "string",
                result: :ok,
                status: "new",
                status_description: "string",
                update_date: ~U[2024-01-26 12:44:55.032Z],
                url: "agent1.site.com"
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
