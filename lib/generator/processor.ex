if Mix.env() in [:dev] do
  defmodule LiqpayAPI.Generator.Processor do
    use OpenAPIClient.Generator.Processor
    alias OpenAPI.Processor.Schema

    @impl OpenAPI.Processor
    def schema_module_and_type(state, %Schema{context: [{:request, module, type, _}]} = schema) do
      OpenAPIClient.Generator.Processor.schema_module_and_type(state, schema)
      module_new = schema_module(module, type, Request)
      {module_new, :t}
    end

    def schema_module_and_type(
          state,
          %Schema{context: [{:response, module, type, _, _}]} = schema
        ) do
      OpenAPIClient.Generator.Processor.schema_module_and_type(state, schema)
      module_new = schema_module(module, type, Response)
      {module_new, :t}
    end

    def schema_module_and_type(state, schema) do
      OpenAPIClient.Generator.Processor.schema_module_and_type(state, schema)
    end

    defp schema_module(module, type, schema_module) do
      type_new = type |> Atom.to_string() |> OpenAPI.Processor.Naming.normalize_identifier(:camel)
      Module.concat([module, type_new, schema_module])
    end

    @impl OpenAPI.Processor
    def schema_format(_state, _schema), do: :struct
  end
end
