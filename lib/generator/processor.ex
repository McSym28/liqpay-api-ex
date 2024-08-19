if Mix.env() in [:dev] do
  defmodule LiqPayAPI.Generator.Processor do
    use OpenAPIClient.Generator.Processor
    alias OpenAPI.Processor.Schema

    @impl OpenAPI.Processor
    def schema_module_and_type(state, schema) do
      state_new =
        case schema do
          %Schema{context: [{:request, module, type, _}]} ->
            process_schema(state, schema, module, type, "Request")

          %Schema{context: [{:response, module, type, _, _}]} ->
            process_schema(state, schema, module, type, "Response")

          _ ->
            state
        end

      OpenAPIClient.Generator.Processor.schema_module_and_type(state_new, schema)
    end

    defp process_schema(
           %OpenAPI.Processor.State{schema_specs_by_ref: schema_specs_by_ref} = state,
           %Schema{ref: schema_ref},
           module,
           type,
           schema_type
         ) do
      type_new = type |> Atom.to_string() |> OpenAPI.Processor.Naming.normalize_identifier(:camel)
      schema_spec = Map.fetch!(schema_specs_by_ref, schema_ref)

      module_new =
        Enum.join([module |> Module.split() |> Enum.join(), type_new, schema_type])

      schema_spec_new = %OpenAPI.Spec.Schema{
        schema_spec
        | "$oag_last_ref_path": ["components", "schemas", module_new]
      }

      schema_specs_by_ref_new = Map.put(schema_specs_by_ref, schema_ref, schema_spec_new)
      %OpenAPI.Processor.State{state | schema_specs_by_ref: schema_specs_by_ref_new}
    end

    @impl OpenAPI.Processor
    def operation_module_names(state, operation_spec) do
      state
      |> OpenAPIClient.Generator.Processor.operation_module_names(operation_spec)
      |> case do
        [InternetAcquiring.Apay] -> [InternetAcquiring.APay]
        [InternetAcquiring.Gpay] -> [InternetAcquiring.GPay]
        [P2pDebit] -> [P2PDebit]
        other -> other
      end
    end

    @impl OpenAPI.Processor
    def schema_format(_state, _schema), do: :struct
  end
end
