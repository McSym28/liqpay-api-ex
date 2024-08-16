if Mix.env() in [:dev] do
  defmodule LiqpayAPI.Generator.TestRenderer do
    use OpenAPIClient.Generator.TestRenderer

    @impl OpenAPIClient.Generator.TestRenderer
    def type_example(_state, {:string, "date-time-liqpay"}, _path), do: "2024-01-02 01:23:45"
    def type_example(_state, {:string, "date-liqpay"}, _path), do: "01.02.2024"
    def type_example(_state, {:string, "month-year-liqpay"}, _path), do: "0124"
    def type_example(_state, {:integer, "timestamp-ms"}, _path), do: 1_706_750_625_987
    def type_example(_state, {:string, "boolean-integer"}, _path), do: "1"
    def type_example(_state, {:string, "boolean-yesno"}, _path), do: "Y"

    def type_example(state, type, path),
      do: OpenAPIClient.Generator.TestRenderer.type_example(state, type, path)
  end
end
