defmodule LiqPayAPIWeb.Callbacks.CallbackControllerTest do
  use LiqPayAPIWeb.ConnCase

  import Mox

  @behaviour_module LiqPayAPI.CallbacksMock
  @client OpenAPIClientMock

  setup :verify_on_exit!

  describe "callback/2" do
    test "[200] processes a request and decodes CallbackRequest from request's body", %{
      conn: conn
    } do
      expect(@client, :callback, &OpenAPIClient.callback/1)

      expect(@behaviour_module, :callback, fn body ->
        assert %LiqPayAPI.Callbacks.CallbackRequest{
                 acq_id: 1,
                 action: :hold,
                 agent_commission: 1.0,
                 amount: 1.0,
                 amount_bonus: 1.0,
                 amount_credit: 1.0,
                 amount_debit: 1.0,
                 authcode_credit: "string",
                 authcode_debit: "string",
                 card_token: "string",
                 commission_credit: 1.0,
                 commission_debit: 1.0,
                 completion_date: ~U[2024-02-01 01:23:45.987Z],
                 create_date: ~U[2024-02-01 01:23:45.987Z],
                 currency: "string",
                 currency_credit: "string",
                 currency_debit: "string",
                 customer: "string",
                 description: "string",
                 end_date: ~U[2024-02-01 01:23:45.987Z],
                 err_code: "string",
                 err_description: "string",
                 err_erc: "string",
                 info: "string",
                 ip: "string",
                 is_3ds: true,
                 liqpay_order_id: "string",
                 mpi_eci: 5,
                 order_id: "string",
                 payment_id: 1,
                 paytype: :card,
                 product_category: "string",
                 product_description: "string",
                 product_name: "string",
                 product_url: "http://example.com",
                 public_key: "string",
                 receiver_commission: 1.0,
                 redirect_to: "string",
                 refund_amount: 1.0,
                 refund_date_last: ~U[2024-02-01 01:23:45.987Z],
                 rrn_credit: "string",
                 rrn_debit: "string",
                 sender_bonus: 1.0,
                 sender_card_bank: "string",
                 sender_card_country: 1,
                 sender_card_mask2: "string",
                 sender_card_type: "string",
                 sender_commission: 1.0,
                 sender_first_name: "string",
                 sender_last_name: "string",
                 sender_phone: "string",
                 status: :"3ds_verify",
                 token: "string",
                 type: "string",
                 verifycode: true,
                 version: 3,
                 wait_reserve_status: true
               } == body

        :ok
      end)

      assert {:ok, body_encoded} =
               Jason.encode(%{
                 "acq_id" => 1,
                 "action" => "hold",
                 "agent_commission" => 1.0,
                 "amount" => 1.0,
                 "amount_bonus" => 1.0,
                 "amount_credit" => 1.0,
                 "amount_debit" => 1.0,
                 "authcode_credit" => "string",
                 "authcode_debit" => "string",
                 "card_token" => "string",
                 "commission_credit" => 1.0,
                 "commission_debit" => 1.0,
                 "completion_date" => 1_706_750_625_987,
                 "create_date" => 1_706_750_625_987,
                 "currency" => "string",
                 "currency_credit" => "string",
                 "currency_debit" => "string",
                 "customer" => "string",
                 "description" => "string",
                 "end_date" => 1_706_750_625_987,
                 "err_code" => "string",
                 "err_description" => "string",
                 "err_erc" => "string",
                 "info" => "string",
                 "ip" => "string",
                 "is_3ds" => true,
                 "liqpay_order_id" => "string",
                 "mpi_eci" => 5,
                 "order_id" => "string",
                 "payment_id" => 1,
                 "paytype" => "card",
                 "product_category" => "string",
                 "product_description" => "string",
                 "product_name" => "string",
                 "product_url" => "http://example.com",
                 "public_key" => "string",
                 "receiver_commission" => 1.0,
                 "redirect_to" => "string",
                 "refund_amount" => 1.0,
                 "refund_date_last" => 1_706_750_625_987,
                 "rrn_credit" => "string",
                 "rrn_debit" => "string",
                 "sender_bonus" => 1.0,
                 "sender_card_bank" => "string",
                 "sender_card_country" => 1,
                 "sender_card_mask2" => "string",
                 "sender_card_type" => "string",
                 "sender_commission" => 1.0,
                 "sender_first_name" => "string",
                 "sender_last_name" => "string",
                 "sender_phone" => "string",
                 "status" => "3ds_verify",
                 "token" => "string",
                 "type" => "string",
                 "verifycode" => "Y",
                 "version" => 3,
                 "wait_reserve_status" => true
               })

      conn =
        conn
        |> Plug.Conn.put_req_header("content-type", "application/x-www-form-urlencoded")
        |> post(
          "/__test__/callbacks/callback",
          LiqPayAPI.Client.Signature.generate_form_data(body_encoded)
        )

      assert response(conn, 200)
    end
  end
end
