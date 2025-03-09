defmodule LiqPayAPI.Information.RegisterTest do
  use ExUnit.Case, async: true
  import Mox

  @httpoison OpenAPIClient.HTTPoisonMock
  @client OpenAPIClientMock

  setup :verify_on_exit!

  describe "compensation_per_transaction/2" do
    test "[200] performs a request, encodes CompensationPerTransaction.Request from request's body and decodes CompensationPerTransaction.Response from response's body" do
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
                  "action" => "register",
                  "date" => "2017-01-31",
                  "format" => "json",
                  "public_key" => "string",
                  "version" => 3
                }} == Jason.decode(body)

        assert {:ok, body_encoded} =
                 Jason.encode(%{
                   "data" => [
                     %{
                       "action" => "pay",
                       "authcode_debit" => "784593",
                       "bonus_type" => "bonusplus",
                       "channel" => "checkout",
                       "create_date" => "2017-01-31 07:51:37",
                       "customer" => "string",
                       "description" => "My product",
                       "end_date" => "2017-01-31 07:51:54",
                       "id" => 107_866,
                       "ip" => "8.8.8.8",
                       "liqpay_order_id" => "J4L8IRG81485849105029639",
                       "order_id" => "98R1U1OV1485849059893399",
                       "paytype" => "liqpay",
                       "sender_card" => "5168 **** **** **16",
                       "sender_card_bank" => "pb",
                       "sender_card_country" => 804,
                       "sender_card_product_type" => "KDV",
                       "sender_card_type" => "mc",
                       "sender_email" => "string",
                       "sender_first_name" => "test",
                       "sender_last_name" => "test",
                       "sender_phone" => "380950000001",
                       "trans_amount" => 1.0,
                       "trans_bonus" => 0.0,
                       "trans_currency" => "UAH",
                       "trans_fee_credit" => 0.03,
                       "trans_fee_debit" => 0.0,
                       "trans_total" => 0.97,
                       "trans_type" => "purchase"
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
              %LiqPayAPI.Information.Register.CompensationPerTransaction.Response{
                data: [
                  %LiqPayAPI.Information.Register.CompensationPerTransaction.Response.Data{
                    action: :pay,
                    authcode_debit: "784593",
                    bonus_type: :bonusplus,
                    channel: :checkout,
                    create_date: ~U[2017-01-31 07:51:37Z],
                    customer: "string",
                    description: "My product",
                    end_date: ~U[2017-01-31 07:51:54Z],
                    id: 107_866,
                    ip: "8.8.8.8",
                    liqpay_order_id: "J4L8IRG81485849105029639",
                    order_id: "98R1U1OV1485849059893399",
                    paytype: "liqpay",
                    sender_card: "5168 **** **** **16",
                    sender_card_bank: "pb",
                    sender_card_country: 804,
                    sender_card_product_type: "KDV",
                    sender_card_type: "mc",
                    sender_email: "string",
                    sender_first_name: "test",
                    sender_last_name: "test",
                    sender_phone: "380950000001",
                    trans_amount: 1.0,
                    trans_bonus: 0.0,
                    trans_currency: "UAH",
                    trans_fee_credit: 0.03,
                    trans_fee_debit: 0.0,
                    trans_total: 0.97,
                    trans_type: :purchase
                  }
                ],
                result: :ok
              }} ==
               LiqPayAPI.Information.Register.compensation_per_transaction(
                 %LiqPayAPI.Information.Register.CompensationPerTransaction.Request{
                   action: :register,
                   date: ~D[2017-01-31],
                   format: "json",
                   public_key: "string",
                   version: 3
                 },
                 private_key: "a4825234f4bae72a0be04eafe9e8e2bada209255",
                 base_url: "https://example.com"
               )
    end
  end

  describe "compensation_report_status/2" do
    test "[200] performs a request, encodes CompensationReportStatus.Request from request's body and decodes CompensationReportStatus.Response from response's body" do
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
                  "action" => "reports_compensation_file_status",
                  "public_key" => "i123456789",
                  "register_token" => "compensation_i123456789_2021.11.23.csv",
                  "version" => 3
                }} == Jason.decode(body)

        assert {:ok, body_encoded} =
                 Jason.encode(%{
                   "filelink" =>
                     "https://liqpay-merchant-reports-eu-west-1-production-1.s3-eu-west-1.amazonaws.com/reports/compensation_i63896222463_2021.11.23.csv?AWSAccessKeyId=ASIA4G3XTFOZJ5WF5PWB&Signature=P8RLb6u6%2BRKJkLRk6HjhQlZQ2T4%3D&Expires=1639738263&x-amz-security-token=IQoJb3JpZ2luX2VjELn%2F%2F%2F%2F%2F%2F%2F%2F%2F%2FwEaCWV1LXdlc3QtMSJIMEYCIQC%2BOZ8y2wDz%2B2a9C8952BazFTCpQpT9OX5cUNF1CcBwzQIhAIPuroQCLJMT9g3PbN%2B4Dk8KXg%2BJb2eZIbYIFYm7A4qdKvADCKL%2F%2F%2F%2F%2F%2F%2F%2F%2F%2FwEQAhoMODM5MzgwMDUyOTE0IgyXUwg7va32HEQ8W5MqxAOghwqrPEpTnH6dLG9toIM8HGhlggL5bFAVI0%2FEE7N1jZRrlyL3455spFAZcAYcROwLnJXzW%2FndQR0QGKKasO6pMW%2B9u8n%2BRUrN6p5kHV4lU9%2BjVb5x2RsMaSjm9Klio8KnB8o5H7CnsTpGEjeAc3ZmeKS10nSSUh4%2FheANMdQVXMCOoba4oqPOxWAIS1dY7tLNrVEgaqhzXhsbK8j0FwwJESYa%2Fq6cN%2FxcnSVq60wUQhRlqaxQr%2FijbXKnprz9Bs0jgvTYF65yPiIKXQ%2B4bYyxR8geOb5c%2BlfFVMFC%2FRyMT1q7r6K7yVuNXxSN3Y%2BeNOR%2FbvDCL9%2Fu3TqNCBHVR3HWPY9P%2BdnF0wjvDS4sDvDEx7V%2Fx%2F%2B52e4flwqgPhAbJrjDqR0dWVuJgwpPfJFjpMVqQ80QPcEEABYY6YycQgNjoSKqdHjLyghykagoV8DAMiNpJciyCb8DMMAv7Cn5%2FCteeYoZevftP%2FIzyZMR9AErQdCTaAL6Db4PAu%2BInbca1sNkWW1HB0DrdonovpFffSXYBHgZMAmij2pD8RmYHvVu9PoPyBR2fRUbZdtoXdaI0Sp7lFlF1hJ1bkgMvjQRPMU0XURvjzCXkvGNBjqkAbiCqn%2F%2BCI%2B9sPSVdCvg7W3oOVsGViKrQmI2SA%2F%2FrJH%2B243LI9iJ7fM4nv5iDTqkpSuy4lYRpX737ZszaJR%2BtRZ1C%2BmGu%2FQjRl0P9mlpCq5qTheOOM9CDvqxu64MiPpgHab4PCATr3i4u8wrTkHhclID4Qx9SU6TyxH0d2OsyngMxjKe3K9wWqkUtQh8FYDKAiPyp3eY99ZGd322XLDdG35CNFCf",
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
              %LiqPayAPI.Information.Register.CompensationReportStatus.Response{
                filelink:
                  "https://liqpay-merchant-reports-eu-west-1-production-1.s3-eu-west-1.amazonaws.com/reports/compensation_i63896222463_2021.11.23.csv?AWSAccessKeyId=ASIA4G3XTFOZJ5WF5PWB&Signature=P8RLb6u6%2BRKJkLRk6HjhQlZQ2T4%3D&Expires=1639738263&x-amz-security-token=IQoJb3JpZ2luX2VjELn%2F%2F%2F%2F%2F%2F%2F%2F%2F%2FwEaCWV1LXdlc3QtMSJIMEYCIQC%2BOZ8y2wDz%2B2a9C8952BazFTCpQpT9OX5cUNF1CcBwzQIhAIPuroQCLJMT9g3PbN%2B4Dk8KXg%2BJb2eZIbYIFYm7A4qdKvADCKL%2F%2F%2F%2F%2F%2F%2F%2F%2F%2FwEQAhoMODM5MzgwMDUyOTE0IgyXUwg7va32HEQ8W5MqxAOghwqrPEpTnH6dLG9toIM8HGhlggL5bFAVI0%2FEE7N1jZRrlyL3455spFAZcAYcROwLnJXzW%2FndQR0QGKKasO6pMW%2B9u8n%2BRUrN6p5kHV4lU9%2BjVb5x2RsMaSjm9Klio8KnB8o5H7CnsTpGEjeAc3ZmeKS10nSSUh4%2FheANMdQVXMCOoba4oqPOxWAIS1dY7tLNrVEgaqhzXhsbK8j0FwwJESYa%2Fq6cN%2FxcnSVq60wUQhRlqaxQr%2FijbXKnprz9Bs0jgvTYF65yPiIKXQ%2B4bYyxR8geOb5c%2BlfFVMFC%2FRyMT1q7r6K7yVuNXxSN3Y%2BeNOR%2FbvDCL9%2Fu3TqNCBHVR3HWPY9P%2BdnF0wjvDS4sDvDEx7V%2Fx%2F%2B52e4flwqgPhAbJrjDqR0dWVuJgwpPfJFjpMVqQ80QPcEEABYY6YycQgNjoSKqdHjLyghykagoV8DAMiNpJciyCb8DMMAv7Cn5%2FCteeYoZevftP%2FIzyZMR9AErQdCTaAL6Db4PAu%2BInbca1sNkWW1HB0DrdonovpFffSXYBHgZMAmij2pD8RmYHvVu9PoPyBR2fRUbZdtoXdaI0Sp7lFlF1hJ1bkgMvjQRPMU0XURvjzCXkvGNBjqkAbiCqn%2F%2BCI%2B9sPSVdCvg7W3oOVsGViKrQmI2SA%2F%2FrJH%2B243LI9iJ7fM4nv5iDTqkpSuy4lYRpX737ZszaJR%2BtRZ1C%2BmGu%2FQjRl0P9mlpCq5qTheOOM9CDvqxu64MiPpgHab4PCATr3i4u8wrTkHhclID4Qx9SU6TyxH0d2OsyngMxjKe3K9wWqkUtQh8FYDKAiPyp3eY99ZGd322XLDdG35CNFCf",
                result: :ok,
                status: :success
              }} ==
               LiqPayAPI.Information.Register.compensation_report_status(
                 %LiqPayAPI.Information.Register.CompensationReportStatus.Request{
                   action: :reports_compensation_file_status,
                   public_key: "i123456789",
                   register_token: "compensation_i123456789_2021.11.23.csv",
                   version: 3
                 },
                 private_key: "a4825234f4bae72a0be04eafe9e8e2bada209255",
                 base_url: "https://example.com"
               )
    end
  end

  describe "compensation_report/2" do
    test "[200] performs a request, encodes CompensationReport.Request from request's body and decodes CompensationReport.Response from response's body" do
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
                  "action" => "reports_compensation_file",
                  "compensation_id" => "string",
                  "date" => "2021-11-23",
                  "public_key" => "i123456789",
                  "resp_format" => "csv",
                  "version" => 3
                }} == Jason.decode(body)

        assert {:ok, body_encoded} =
                 Jason.encode(%{
                   "register_token" => "compensation_i123456789_2021.11.23.csv",
                   "result" => "ok",
                   "status" => "processing"
                 })

        {:ok,
         %HTTPoison.Response{
           status_code: 200,
           headers: [{"Content-Type", "application/json"}],
           body: body_encoded
         }}
      end)

      assert {:ok,
              %LiqPayAPI.Information.Register.CompensationReport.Response{
                register_token: "compensation_i123456789_2021.11.23.csv",
                result: "ok",
                status: "processing"
              }} ==
               LiqPayAPI.Information.Register.compensation_report(
                 %LiqPayAPI.Information.Register.CompensationReport.Request{
                   action: :reports_compensation_file,
                   compensation_id: "string",
                   date: ~D[2021-11-23],
                   public_key: "i123456789",
                   resp_format: :csv,
                   version: 3
                 },
                 private_key: "a4825234f4bae72a0be04eafe9e8e2bada209255",
                 base_url: "https://example.com"
               )
    end
  end

  describe "compensation_report_p2p/2" do
    test "[200] performs a request, encodes CompensationReportP2P.Request from request's body and decodes CompensationReportP2P.Response from response's body" do
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
                  "action" => "reports_compensation_file",
                  "date" => "2021-11-23",
                  "public_key" => "i123456789",
                  "resp_format" => "csv",
                  "type" => "p2p",
                  "version" => 3
                }} == Jason.decode(body)

        assert {:ok, body_encoded} =
                 Jason.encode(%{
                   "register_token" => "register_i11301247803_105267.csv",
                   "result" => "ok",
                   "status" => "processing"
                 })

        {:ok,
         %HTTPoison.Response{
           status_code: 200,
           headers: [{"Content-Type", "application/json"}],
           body: body_encoded
         }}
      end)

      assert {:ok,
              %LiqPayAPI.Information.Register.CompensationReportP2P.Response{
                register_token: "register_i11301247803_105267.csv",
                result: :ok,
                status: "processing"
              }} ==
               LiqPayAPI.Information.Register.compensation_report_p2p(
                 %LiqPayAPI.Information.Register.CompensationReportP2P.Request{
                   action: :reports_compensation_file,
                   date: ~D[2021-11-23],
                   public_key: "i123456789",
                   resp_format: :csv,
                   type: :p2p,
                   version: 3
                 },
                 private_key: "a4825234f4bae72a0be04eafe9e8e2bada209255",
                 base_url: "https://example.com"
               )
    end
  end

  describe "compensation_report_p2p_status/2" do
    test "[200] performs a request, encodes CompensationReportP2PStatus.Request from request's body and decodes CompensationReportP2PStatus.Response from response's body" do
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
                  "action" => "reports_compensation_file_status",
                  "public_key" => "i123456789",
                  "register_token" => "compensation_i123456789_2021.11.23.csv",
                  "version" => 3
                }} == Jason.decode(body)

        assert {:ok, body_encoded} =
                 Jason.encode(%{
                   "filelink" =>
                     "https://liqpay-merchant-reports-eu-west-1-production-1.s3-eu-west-1.amazonaws.com/reports/compensation_i63896222463_2021.11.23.csv?AWSAccessKeyId=ASIA4G3XTFOZJ5WF5PWB&Signature=P8RLb6u6%2BRKJkLRk6HjhQlZQ2T4%3D&Expires=1639738263&x-amz-security-token=IQoJb3JpZ2luX2VjELn%2F%2F%2F%2F%2F%2F%2F%2F%2F%2FwEaCWV1LXdlc3QtMSJIMEYCIQC%2BOZ8y2wDz%2B2a9C8952BazFTCpQpT9OX5cUNF1CcBwzQIhAIPuroQCLJMT9g3PbN%2B4Dk8KXg%2BJb2eZIbYIFYm7A4qdKvADCKL%2F%2F%2F%2F%2F%2F%2F%2F%2F%2FwEQAhoMODM5MzgwMDUyOTE0IgyXUwg7va32HEQ8W5MqxAOghwqrPEpTnH6dLG9toIM8HGhlggL5bFAVI0%2FEE7N1jZRrlyL3455spFAZcAYcROwLnJXzW%2FndQR0QGKKasO6pMW%2B9u8n%2BRUrN6p5kHV4lU9%2BjVb5x2RsMaSjm9Klio8KnB8o5H7CnsTpGEjeAc3ZmeKS10nSSUh4%2FheANMdQVXMCOoba4oqPOxWAIS1dY7tLNrVEgaqhzXhsbK8j0FwwJESYa%2Fq6cN%2FxcnSVq60wUQhRlqaxQr%2FijbXKnprz9Bs0jgvTYF65yPiIKXQ%2B4bYyxR8geOb5c%2BlfFVMFC%2FRyMT1q7r6K7yVuNXxSN3Y%2BeNOR%2FbvDCL9%2Fu3TqNCBHVR3HWPY9P%2BdnF0wjvDS4sDvDEx7V%2Fx%2F%2B52e4flwqgPhAbJrjDqR0dWVuJgwpPfJFjpMVqQ80QPcEEABYY6YycQgNjoSKqdHjLyghykagoV8DAMiNpJciyCb8DMMAv7Cn5%2FCteeYoZevftP%2FIzyZMR9AErQdCTaAL6Db4PAu%2BInbca1sNkWW1HB0DrdonovpFffSXYBHgZMAmij2pD8RmYHvVu9PoPyBR2fRUbZdtoXdaI0Sp7lFlF1hJ1bkgMvjQRPMU0XURvjzCXkvGNBjqkAbiCqn%2F%2BCI%2B9sPSVdCvg7W3oOVsGViKrQmI2SA%2F%2FrJH%2B243LI9iJ7fM4nv5iDTqkpSuy4lYRpX737ZszaJR%2BtRZ1C%2BmGu%2FQjRl0P9mlpCq5qTheOOM9CDvqxu64MiPpgHab4PCATr3i4u8wrTkHhclID4Qx9SU6TyxH0d2OsyngMxjKe3K9wWqkUtQh8FYDKAiPyp3eY99ZGd322XLDdG35CNFCf",
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
              %LiqPayAPI.Information.Register.CompensationReportP2PStatus.Response{
                filelink:
                  "https://liqpay-merchant-reports-eu-west-1-production-1.s3-eu-west-1.amazonaws.com/reports/compensation_i63896222463_2021.11.23.csv?AWSAccessKeyId=ASIA4G3XTFOZJ5WF5PWB&Signature=P8RLb6u6%2BRKJkLRk6HjhQlZQ2T4%3D&Expires=1639738263&x-amz-security-token=IQoJb3JpZ2luX2VjELn%2F%2F%2F%2F%2F%2F%2F%2F%2F%2FwEaCWV1LXdlc3QtMSJIMEYCIQC%2BOZ8y2wDz%2B2a9C8952BazFTCpQpT9OX5cUNF1CcBwzQIhAIPuroQCLJMT9g3PbN%2B4Dk8KXg%2BJb2eZIbYIFYm7A4qdKvADCKL%2F%2F%2F%2F%2F%2F%2F%2F%2F%2FwEQAhoMODM5MzgwMDUyOTE0IgyXUwg7va32HEQ8W5MqxAOghwqrPEpTnH6dLG9toIM8HGhlggL5bFAVI0%2FEE7N1jZRrlyL3455spFAZcAYcROwLnJXzW%2FndQR0QGKKasO6pMW%2B9u8n%2BRUrN6p5kHV4lU9%2BjVb5x2RsMaSjm9Klio8KnB8o5H7CnsTpGEjeAc3ZmeKS10nSSUh4%2FheANMdQVXMCOoba4oqPOxWAIS1dY7tLNrVEgaqhzXhsbK8j0FwwJESYa%2Fq6cN%2FxcnSVq60wUQhRlqaxQr%2FijbXKnprz9Bs0jgvTYF65yPiIKXQ%2B4bYyxR8geOb5c%2BlfFVMFC%2FRyMT1q7r6K7yVuNXxSN3Y%2BeNOR%2FbvDCL9%2Fu3TqNCBHVR3HWPY9P%2BdnF0wjvDS4sDvDEx7V%2Fx%2F%2B52e4flwqgPhAbJrjDqR0dWVuJgwpPfJFjpMVqQ80QPcEEABYY6YycQgNjoSKqdHjLyghykagoV8DAMiNpJciyCb8DMMAv7Cn5%2FCteeYoZevftP%2FIzyZMR9AErQdCTaAL6Db4PAu%2BInbca1sNkWW1HB0DrdonovpFffSXYBHgZMAmij2pD8RmYHvVu9PoPyBR2fRUbZdtoXdaI0Sp7lFlF1hJ1bkgMvjQRPMU0XURvjzCXkvGNBjqkAbiCqn%2F%2BCI%2B9sPSVdCvg7W3oOVsGViKrQmI2SA%2F%2FrJH%2B243LI9iJ7fM4nv5iDTqkpSuy4lYRpX737ZszaJR%2BtRZ1C%2BmGu%2FQjRl0P9mlpCq5qTheOOM9CDvqxu64MiPpgHab4PCATr3i4u8wrTkHhclID4Qx9SU6TyxH0d2OsyngMxjKe3K9wWqkUtQh8FYDKAiPyp3eY99ZGd322XLDdG35CNFCf",
                result: :ok,
                status: :success
              }} ==
               LiqPayAPI.Information.Register.compensation_report_p2p_status(
                 %LiqPayAPI.Information.Register.CompensationReportP2PStatus.Request{
                   action: :reports_compensation_file_status,
                   public_key: "i123456789",
                   register_token: "compensation_i123456789_2021.11.23.csv",
                   version: 3
                 },
                 private_key: "a4825234f4bae72a0be04eafe9e8e2bada209255",
                 base_url: "https://example.com"
               )
    end
  end

  describe "compensation_per_day/2" do
    test "[200] performs a request, encodes CompensationPerDay.Request from request's body and decodes CompensationPerDay.Response from response's body" do
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
                  "action" => "reports_compensation",
                  "compensation_id" => "200000007",
                  "date" => "2024-01-02",
                  "public_key" => "string",
                  "resp_format" => "json",
                  "version" => 3
                }} == Jason.decode(body)

        assert {:ok, body_encoded} =
                 Jason.encode(%{
                   "data" => [
                     %{
                       "action" => "pay",
                       "authcode_debit" => "784593",
                       "bonus_type" => "bonusplus",
                       "channel" => "checkout",
                       "create_date" => "2017-01-31 07:51:37",
                       "customer" => "string",
                       "description" => "My product",
                       "end_date" => "2017-01-31 07:51:54",
                       "id" => 107_866,
                       "ip" => "8.8.8.8",
                       "liqpay_order_id" => "J4L8IRG81485849105029639",
                       "order_id" => "98R1U1OV1485849059893399",
                       "paytype" => "liqpay",
                       "sender_card" => "5168 **** **** **16",
                       "sender_card_bank" => "pb",
                       "sender_card_country" => 804,
                       "sender_card_product_type" => "KDV",
                       "sender_card_type" => "mc",
                       "sender_email" => "string",
                       "sender_first_name" => "test",
                       "sender_last_name" => "test",
                       "sender_phone" => "380950000001",
                       "trans_amount" => 1.0,
                       "trans_bonus" => 0.0,
                       "trans_currency" => "UAH",
                       "trans_fee_credit" => 0.03,
                       "trans_fee_debit" => 0.0,
                       "trans_total" => 0.97,
                       "trans_type" => "purchase"
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
              %LiqPayAPI.Information.Register.CompensationPerDay.Response{
                data: [
                  %LiqPayAPI.Information.Register.CompensationPerDay.Response.Data.Full{
                    action: :pay,
                    authcode_debit: "784593",
                    bonus_type: :bonusplus,
                    channel: :checkout,
                    create_date: ~U[2017-01-31 07:51:37Z],
                    customer: "string",
                    description: "My product",
                    end_date: ~U[2017-01-31 07:51:54Z],
                    id: 107_866,
                    ip: "8.8.8.8",
                    liqpay_order_id: "J4L8IRG81485849105029639",
                    order_id: "98R1U1OV1485849059893399",
                    paytype: "liqpay",
                    sender_card: "5168 **** **** **16",
                    sender_card_bank: "pb",
                    sender_card_country: 804,
                    sender_card_product_type: "KDV",
                    sender_card_type: "mc",
                    sender_email: "string",
                    sender_first_name: "test",
                    sender_last_name: "test",
                    sender_phone: "380950000001",
                    trans_amount: 1.0,
                    trans_bonus: 0.0,
                    trans_currency: "UAH",
                    trans_fee_credit: 0.03,
                    trans_fee_debit: 0.0,
                    trans_total: 0.97,
                    trans_type: :purchase
                  }
                ],
                result: :ok
              }} ==
               LiqPayAPI.Information.Register.compensation_per_day(
                 %LiqPayAPI.Information.Register.CompensationPerDay.Request{
                   action: :reports_compensation,
                   compensation_id: "200000007",
                   date: ~D[2024-01-02],
                   public_key: "string",
                   resp_format: :json,
                   version: 3
                 },
                 private_key: "a4825234f4bae72a0be04eafe9e8e2bada209255",
                 base_url: "https://example.com"
               )
    end
  end
end
