if Mix.env() in [:dev, :test] do
  defmodule Liqpay.Generator.ExampleGenerator do
    alias OpenAPIClient.Generator.ExampleGenerator

    @behaviour ExampleGenerator

    @doc """
    Generate example value for a specific type

    ## Examples

        iex> datetime_string = #{__MODULE__}.generate({:string, "date-time-liqpay"}, [], #{__MODULE__})
        iex> is_binary(datetime_string)
        true
        iex> date_string = #{__MODULE__}.generate({:string, "date-liqpay"}, [], #{__MODULE__})
        iex> is_binary(date_string)
        true
        iex> date_string = #{__MODULE__}.generate({:string, "month-year-liqpay"}, [], #{__MODULE__})
        iex> is_binary(date_string)
        true
        iex> timestamp = #{__MODULE__}.generate({:integer, "timestamp-ms"}, [], #{__MODULE__})
        iex> is_integer(timestamp)
        true
        iex> integer_boolean_string = #{__MODULE__}.generate({:string, "boolean-integer"}, [], #{__MODULE__})
        iex> is_binary(integer_boolean_string)
        true
        iex> yesno_boolean_string = #{__MODULE__}.generate({:string, "boolean-yesno"}, [], #{__MODULE__})
        iex> is_binary(yesno_boolean_string)
        true

    """
    @impl ExampleGenerator
    def generate({:string, "date-time-liqpay"}, _path, _caller_module), do: "2024-01-02 01:23:45"
    def generate({:string, "date-liqpay"}, _path, _caller_module), do: "01.02.2024"
    def generate({:string, "month-year-liqpay"}, _path, _caller_module), do: "0124"
    def generate({:integer, "timestamp-ms"}, _path, _caller_module), do: 1_706_750_625_987
    def generate({:string, "boolean-integer"}, _path, _caller_module), do: "1"
    def generate({:string, "boolean-yesno"}, _path, _caller_module), do: "Y"

    def generate(type, path, caller_module),
      do: ExampleGenerator.generate(type, path, caller_module)
  end
end
