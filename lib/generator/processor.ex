if Mix.env() in [:dev] do
  defmodule LiqpayAPI.Generator.Processor do
    use OpenAPIClient.Generator.Processor
    alias OpenAPI.Processor.Schema

    @impl OpenAPI.Processor
    def schema_module_and_type(state, %Schema{context: [{:request, _, _, _}]} = schema) do
      OpenAPIClient.Generator.Processor.schema_module_and_type(state, schema)
      {Request, :t}
    end

    def schema_module_and_type(state, %Schema{context: [{:response, _, _, _, _}]} = schema) do
      OpenAPIClient.Generator.Processor.schema_module_and_type(state, schema)
      {Response, :t}
    end

    def schema_module_and_type(state, schema) do
      OpenAPIClient.Generator.Processor.schema_module_and_type(state, schema)
      |> tap(&IO.inspect(schema, label: inspect(&1)))
    end

    @impl OpenAPI.Processor
    def schema_format(_state, _schema), do: :struct
  end
end
