if Mix.env() in [:dev] do
  defmodule LiqPayAPI.Generator.Processor do
    use OpenAPIClient.Generator.Processor
    alias OpenAPI.Processor.Schema

    @impl OpenAPI.Processor
    def schema_module_and_type(
          %OpenAPI.Processor.State{schema_specs_by_ref: schema_specs_by_ref} = state,
          %Schema{ref: schema_ref} = schema
        ) do
      %OpenAPI.Spec.Schema{"$oag_last_ref_path": ref_path} =
        schema_spec = Map.fetch!(schema_specs_by_ref, schema_ref)

      suffix =
        ref_path
        |> Enum.take(-2)
        |> case do
          ["oneOf", index] -> "_one_of_#{index}"
          _ -> ""
        end

      {state_new, schema_new} =
        case schema do
          %Schema{context: [{:request, module, type, _}]} ->
            state_new =
              update_state(state, schema, schema_spec, module, type, "_request#{suffix}")

            {state_new, schema}

          %Schema{context: [{:response, module, type, _, _}]} ->
            state_new =
              update_state(state, schema, schema_spec, module, type, "_response#{suffix}")

            {state_new, schema}

          %Schema{context: [{:field, parent_ref, name}]} ->
            name_new = "#{name}#{suffix}"
            schema_new = %Schema{schema | context: [{:field, parent_ref, name_new}]}
            {state, schema_new}

          _ ->
            {state, schema}
        end

      OpenAPIClient.Generator.Processor.schema_module_and_type(state_new, schema_new)
    end

    defp update_state(
           %OpenAPI.Processor.State{schema_specs_by_ref: schema_specs_by_ref} = state,
           %Schema{ref: schema_ref} = _schema,
           schema_spec,
           module,
           type,
           schema_suffix
         ) do
      type_new = type |> Atom.to_string() |> OpenAPI.Processor.Naming.normalize_identifier(:camel)
      module_string = module |> Module.split() |> Enum.join()
      module_new = Enum.join([module_string, type_new, schema_suffix])

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
