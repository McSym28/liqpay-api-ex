defmodule Liqpay.Client.Signature do
  require Logger

  @spec generate_form_data(String.t(), String.t()) :: String.t()
  def generate_form_data(json_string, private_key) do
    data = Base.encode64(json_string)
    signature = generate_signature(data, private_key)
    URI.encode_query(%{data: data, signature: signature})
  end

  @spec check?(String.t(), String.t(), String.t()) :: boolean()
  def check?(data, private_key, signature) do
    generated_signature = generate_signature(data, private_key)

    if signature == generated_signature do
      true
    else
      Logger.debug(
        "Signature didn't match: #{inspect(generated_signature)} != #{inspect(signature)}"
      )

      false
    end
  end

  defp generate_signature(data, private_key) do
    sign_string = "#{private_key}#{data}#{private_key}"
    :sha |> :crypto.hash(sign_string) |> Base.encode64()
  end
end
