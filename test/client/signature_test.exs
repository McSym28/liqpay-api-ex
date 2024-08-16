defmodule LiqpayAPI.Client.SignatureTest do
  use ExUnit.Case, async: true
  alias LiqpayAPI.Client.Signature

  describe "generate_form_data/2" do
    test "generates form data from JSON string" do
      json_string =
        ~s<{"public_key":"i00000000","version":"3","action":"pay","amount":"3","currency":"UAH","description":"test","order_id":"000001"}>

      private_key = "a4825234f4bae72a0be04eafe9e8e2bada209255"

      data =
        "eyJwdWJsaWNfa2V5IjoiaTAwMDAwMDAwIiwidmVyc2lvbiI6IjMiLCJhY3Rpb24iOiJwYXkiLCJhbW91bnQiOiIzIiwiY3VycmVuY3kiOiJVQUgiLCJkZXNjcmlwdGlvbiI6InRlc3QiLCJvcmRlcl9pZCI6IjAwMDAwMSJ9"

      signature = "wR+UZDC4jjeL/qUOvIsofIWpZh8="

      assert "data=#{URI.encode_www_form(data)}&signature=#{URI.encode_www_form(signature)}" ==
               Signature.generate_form_data(json_string, private_key)
    end
  end

  describe "check?/3" do
    test "returns true for a valid signature" do
      data = "base64_post_string"
      private_key = "your_private_key"
      signature = "tp+ZLmKm1/E83dIzUpx5ljcttP4="
      assert Signature.check?(data, private_key, signature)
    end

    test "returns false for an invalid signature" do
      data = "base64_post_string"
      private_key = "wrong_private_key"
      signature = "tp+ZLmKm1/E83dIzUpx5ljcttP4="
      refute Signature.check?(data, private_key, signature)
    end
  end
end
