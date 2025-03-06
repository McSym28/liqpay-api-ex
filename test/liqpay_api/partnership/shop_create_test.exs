defmodule LiqPayAPI.Partnership.ShopCreateTest do
  use ExUnit.Case, async: true
  import Mox

  @httpoison OpenAPIClient.HTTPoisonMock
  @client OpenAPIClientMock

  setup :verify_on_exit!

  describe "create/2" do
    test "[200] performs a request, encodes Create.Request from request's body and decodes Create.Response from response's body" do
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
                  "action" => "agent_shop_create",
                  "amount_procent_agent" => 1.0,
                  "amount_static_agent" => 1.0,
                  "can_checkout_edit" => true,
                  "can_reports" => true,
                  "company" => "string",
                  "currency_static_agent" => "string",
                  "description" => "site description",
                  "email" => "email@gmail.com",
                  "iban" => "string",
                  "law_co_owners_info" => [
                    %{
                      "birth_date" => "2024-01-02",
                      "citizenship" => "Ukraine",
                      "company_name" => "string",
                      "company_okpo" => "string",
                      "inn" => "string",
                      "name" => "string",
                      "residency" => "string",
                      "share_in_capital" => "string"
                    }
                  ],
                  "law_contacts" => %{"email" => "string", "phone" => "string"},
                  "law_cto_info" => %{
                    "birth_date" => "2024-01-02",
                    "citizenship" => "Ukraine",
                    "inn" => "string",
                    "name" => "string",
                    "residency" => "string"
                  },
                  "law_iban" => "string",
                  "law_name" => "string",
                  "law_okpo" => "string",
                  "law_owners_info" => [
                    %{
                      "birth_date" => "2024-01-02",
                      "citizenship" => "Ukraine",
                      "inn" => "string",
                      "name" => "string",
                      "residency" => "string",
                      "share_in_capital" => "string"
                    }
                  ],
                  "logo" => "string",
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
              %LiqPayAPI.Partnership.ShopCreate.Create.Response{
                private_key: "wGsqoko5412LcD0vB215XK2wQSgLDVBrsaPIRi6",
                public_key: "i000000000",
                status: :success
              }} ==
               LiqPayAPI.Partnership.ShopCreate.create(
                 %LiqPayAPI.Partnership.ShopCreate.Create.Request{
                   action: :agent_shop_create,
                   aggregator: %LiqPayAPI.Partnership.ShopCreate.Create.Request.Aggregator{
                     law_co_owners_info: [
                       %LiqPayAPI.Partnership.ShopCreate.Create.Request.Aggregator.LawCoOwnersInfo{
                         birth_date: ~D[2024-01-02],
                         citizenship: "Ukraine",
                         company_name: "string",
                         company_okpo: "string",
                         inn: "string",
                         name: "string",
                         residency: "string",
                         share_in_capital: "string"
                       }
                     ],
                     law_contacts:
                       %LiqPayAPI.Partnership.ShopCreate.Create.Request.Aggregator.LawContacts{
                         email: "string",
                         phone: "string"
                       },
                     law_cto_info:
                       %LiqPayAPI.Partnership.ShopCreate.Create.Request.Aggregator.LawCTOInfo{
                         birth_date: ~D[2024-01-02],
                         citizenship: "Ukraine",
                         inn: "string",
                         name: "string",
                         residency: "string"
                       },
                     law_iban: "string",
                     law_name: "string",
                     law_okpo: "string",
                     law_owners_info: [
                       %LiqPayAPI.Partnership.ShopCreate.Create.Request.Aggregator.LawOwnersInfo{
                         birth_date: ~D[2024-01-02],
                         citizenship: "Ukraine",
                         inn: "string",
                         name: "string",
                         residency: "string",
                         share_in_capital: "string"
                       }
                     ]
                   },
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

  describe "documents/2" do
    test "[200] performs a request, encodes Documents.Request from request's body and decodes Documents.Response from response's body" do
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
                  "action" => "agent_info_mcc_docs",
                  "language" => "uk",
                  "mcc_code" => 1.0,
                  "public_key" => "string",
                  "version" => 3
                }} == Jason.decode(body)

        assert {:ok, body_encoded} =
                 Jason.encode(%{
                   "expected_docs" => [
                     %{
                       "alt_docs" => ["string"],
                       "description" => "string",
                       "doc_id" => 1.0,
                       "doc_type" => "string",
                       "name" => "string"
                     }
                   ],
                   "result" => "error",
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
              %LiqPayAPI.Partnership.ShopCreate.Documents.Response{
                expected_docs: [
                  %LiqPayAPI.Partnership.ShopCreate.Documents.Response.ExpectedDocs{
                    alt_docs: ["string"],
                    description: "string",
                    doc_id: 1.0,
                    doc_type: "string",
                    name: "string"
                  }
                ],
                result: :error,
                status: "string"
              }} ==
               LiqPayAPI.Partnership.ShopCreate.documents(
                 %LiqPayAPI.Partnership.ShopCreate.Documents.Request{
                   action: :agent_info_mcc_docs,
                   language: :uk,
                   mcc_code: 1.0,
                   public_key: "string",
                   version: 3
                 },
                 private_key: "a4825234f4bae72a0be04eafe9e8e2bada209255",
                 base_url: "https://example.com"
               )
    end
  end

  describe "available_mcc/2" do
    test "[200] performs a request, encodes AvailableMCC.Request from request's body and decodes AvailableMCC.Response from response's body" do
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
                  "action" => "agent_info_mcc_codes",
                  "language" => "uk",
                  "public_key" => "string",
                  "version" => 3
                }} == Jason.decode(body)

        assert {:ok, body_encoded} =
                 Jason.encode(%{
                   "mcc_codes" => [
                     %{"id" => 1.0, "mcc_code" => 1.0, "name" => "string", "parent_id" => 1.0}
                   ],
                   "result" => "error",
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
              %LiqPayAPI.Partnership.ShopCreate.AvailableMCC.Response{
                mcc_codes: [
                  %LiqPayAPI.Partnership.ShopCreate.AvailableMCC.Response.MCCCodes{
                    id: 1.0,
                    mcc_code: 1.0,
                    name: "string",
                    parent_id: 1.0
                  }
                ],
                result: :error,
                status: "string"
              }} ==
               LiqPayAPI.Partnership.ShopCreate.available_mcc(
                 %LiqPayAPI.Partnership.ShopCreate.AvailableMCC.Request{
                   action: :agent_info_mcc_codes,
                   language: :uk,
                   public_key: "string",
                   version: 3
                 },
                 private_key: "a4825234f4bae72a0be04eafe9e8e2bada209255",
                 base_url: "https://example.com"
               )
    end
  end

  describe "register/2" do
    test "[200] performs a request, encodes Register.Request from request's body and decodes Register.Response from response's body" do
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
                  "action" => "agent_shop_register",
                  "amount_procent_agent" => 1.0,
                  "amount_static_agent" => 1.0,
                  "can_checkout_edit" => true,
                  "can_reports" => true,
                  "company" => "string",
                  "currency_static_agent" => "string",
                  "description" => "site description",
                  "docs" => [
                    %{
                      "doc_id" => 1.0,
                      "file" => "string",
                      "file_name" => "string",
                      "name" => "string"
                    }
                  ],
                  "email" => "email@gmail.com",
                  "facebook" => "string",
                  "iban" => "string",
                  "instagram" => "string",
                  "law_co_owners_info" => [
                    %{
                      "birth_date" => "2024-01-02",
                      "citizenship" => "Ukraine",
                      "company_name" => "string",
                      "company_okpo" => "string",
                      "inn" => "string",
                      "name" => "string",
                      "residency" => "string",
                      "share_in_capital" => "string"
                    }
                  ],
                  "law_contacts" => %{"email" => "string", "phone" => "string"},
                  "law_cto_info" => %{
                    "birth_date" => "2024-01-02",
                    "citizenship" => "Ukraine",
                    "inn" => "string",
                    "name" => "string",
                    "residency" => "string"
                  },
                  "law_iban" => "string",
                  "law_name" => "string",
                  "law_okpo" => "string",
                  "law_owners_info" => [
                    %{
                      "birth_date" => "2024-01-02",
                      "citizenship" => "Ukraine",
                      "inn" => "string",
                      "name" => "string",
                      "residency" => "string",
                      "share_in_capital" => "string"
                    }
                  ],
                  "logo" => "string",
                  "mcc_code" => "string",
                  "name" => "Site name",
                  "okpo" => "string",
                  "phone" => "+380950000001",
                  "public_key" => "string",
                  "telegram" => "string",
                  "url_app_android" => "string",
                  "url_app_iphone" => "string",
                  "url_callback_status" => "string",
                  "url_offer" => "string",
                  "url_site" => "agent1.site.com",
                  "version" => 3,
                  "viber" => "string"
                }} == Jason.decode(body)

        assert {:ok, body_encoded} =
                 Jason.encode(%{
                   "activation_status" => 1.0,
                   "company_name" => "string",
                   "create_date" => "2024-01-02",
                   "email" => "string",
                   "name" => "string",
                   "okpo" => "string",
                   "phone" => "string",
                   "public_key" => "string",
                   "refund_number" => "string",
                   "update_date" => "2024-01-02",
                   "url" => "string"
                 })

        {:ok,
         %HTTPoison.Response{
           status_code: 200,
           headers: [{"Content-Type", "application/json"}],
           body: body_encoded
         }}
      end)

      assert {:ok,
              %LiqPayAPI.Partnership.ShopCreate.Register.Response{
                activation_status: 1.0,
                company_name: "string",
                create_date: ~D[2024-01-02],
                email: "string",
                name: "string",
                okpo: "string",
                phone: "string",
                public_key: "string",
                refund_number: "string",
                update_date: ~D[2024-01-02],
                url: "string"
              }} ==
               LiqPayAPI.Partnership.ShopCreate.register(
                 %LiqPayAPI.Partnership.ShopCreate.Register.Request{
                   action: :agent_shop_register,
                   aggregator: %LiqPayAPI.Partnership.ShopCreate.Register.Request.Aggregator{
                     law_co_owners_info: [
                       %LiqPayAPI.Partnership.ShopCreate.Register.Request.Aggregator.LawCoOwnersInfo{
                         birth_date: ~D[2024-01-02],
                         citizenship: "Ukraine",
                         company_name: "string",
                         company_okpo: "string",
                         inn: "string",
                         name: "string",
                         residency: "string",
                         share_in_capital: "string"
                       }
                     ],
                     law_contacts:
                       %LiqPayAPI.Partnership.ShopCreate.Register.Request.Aggregator.LawContacts{
                         email: "string",
                         phone: "string"
                       },
                     law_cto_info:
                       %LiqPayAPI.Partnership.ShopCreate.Register.Request.Aggregator.LawCTOInfo{
                         birth_date: ~D[2024-01-02],
                         citizenship: "Ukraine",
                         inn: "string",
                         name: "string",
                         residency: "string"
                       },
                     law_iban: "string",
                     law_name: "string",
                     law_okpo: "string",
                     law_owners_info: [
                       %LiqPayAPI.Partnership.ShopCreate.Register.Request.Aggregator.LawOwnersInfo{
                         birth_date: ~D[2024-01-02],
                         citizenship: "Ukraine",
                         inn: "string",
                         name: "string",
                         residency: "string",
                         share_in_capital: "string"
                       }
                     ]
                   },
                   amount_procent_agent: 1.0,
                   amount_static_agent: 1.0,
                   can_checkout_edit: true,
                   can_reports: true,
                   company: "string",
                   currency_static_agent: "string",
                   description: "site description",
                   docs: [
                     %LiqPayAPI.Partnership.ShopCreate.Register.Request.Docs{
                       doc_id: 1.0,
                       file: "string",
                       file_name: "string",
                       name: "string"
                     }
                   ],
                   email: "email@gmail.com",
                   facebook: "string",
                   iban: "string",
                   instagram: "string",
                   logo: "string",
                   mcc_code: "string",
                   name: "Site name",
                   okpo: "string",
                   phone: "+380950000001",
                   public_key: "string",
                   telegram: "string",
                   url_app_android: "string",
                   url_app_iphone: "string",
                   url_callback_status: "string",
                   url_offer: "string",
                   url_site: "agent1.site.com",
                   version: 3,
                   viber: "string"
                 },
                 private_key: "a4825234f4bae72a0be04eafe9e8e2bada209255",
                 base_url: "https://example.com"
               )
    end
  end
end
