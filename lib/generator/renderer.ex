if Mix.env() in [:dev] do
  defmodule LiqPayAPI.Generator.Renderer do
    use OpenAPIClient.Generator.Renderer
    alias OpenAPI.Processor.Operation
    alias OpenAPI.Renderer.File
    alias OpenAPIClient.Generator.Schema, as: GeneratorSchema
    alias OpenAPIClient.Generator.Field, as: GeneratorField

    @impl OpenAPI.Renderer
    def render_type(_state, {:string, "date-time-liqpay"}), do: quote(do: DateTime.t())
    def render_type(_state, {:string, "date-liqpay"}), do: quote(do: Date.t())
    def render_type(_state, {:string, "month-year-liqpay"}), do: quote(do: Date.t())
    def render_type(_state, {:integer, "timestamp-ms"}), do: quote(do: DateTime.t())
    def render_type(_state, {:string, "boolean-integer"}), do: quote(do: boolean())
    def render_type(_state, {:string, "boolean-yesno"}), do: quote(do: boolean())
    def render_type(state, type), do: OpenAPIClient.Generator.Renderer.render_type(state, type)

    @impl OpenAPI.Renderer
    def render_default_client(state, %File{module: Public} = file) do
      state
      |> OpenAPIClient.Generator.Renderer.render_default_client(file)
      |> Macro.prewalk(fn
        {:base_url, base_url_metadata, [_url]} ->
          {:base_url, base_url_metadata, ["https://api.privatbank.ua"]}

        expression ->
          expression
      end)
    end

    def render_default_client(state, file),
      do: OpenAPIClient.Generator.Renderer.render_default_client(state, file)

    @impl OpenAPI.Renderer
    def render(_state, %File{module: Operations} = _file), do: nil

    def render(state, file), do: OpenAPIClient.Generator.Renderer.render(state, file)

    @impl OpenAPI.Renderer
    def render_operation_function(
          %OpenAPI.Renderer.State{schemas: schemas} = state,
          %Operation{request_body: request_body} = operation
        ) do
      state
      |> OpenAPIClient.Generator.Renderer.render_operation_function(operation)
      |> Macro.prewalk(fn
        {:def, def_metadata,
         [
           {_function, _function_metadata, function_arguments} = function_header,
           [do: {:__block__, block_metadata, function_body}]
         ]} = expression ->
          function_arguments
          |> Enum.any?(fn
            {:body, _, _} -> true
            _ -> false
          end)
          |> if do
            Enum.flat_map(request_body, fn
              {_content_type, schema_ref} when is_reference(schema_ref) ->
                [{_, %GeneratorSchema{fields: generator_fields} = _generator_schema}] =
                  :ets.lookup(:schemas, schema_ref)

                %OpenAPI.Processor.Schema{module_name: module} = Map.fetch!(schemas, schema_ref)

                generator_fields
                |> Enum.find(fn
                  %GeneratorField{old_name: "public_key"} = field -> field
                  _ -> nil
                end)
                |> case do
                  %GeneratorField{
                    schema_type: %OpenAPIClient.Generator.SchemaType{default: default}
                  }
                  when not is_nil(default) ->
                    module_new =
                      state
                      |> OpenAPIClient.Generator.Utils.get_oapi_generator_config(:base_module, "")
                      |> Module.concat(module)

                    [
                      {:->, [],
                       [
                         [quote(do: %unquote(module_new){public_key: nil})],
                         quote(do: %unquote(module_new){body | public_key: unquote(default)})
                       ]}
                    ]

                  _ ->
                    []
                end

              _ ->
                []
            end)
          else
            []
          end
          |> case do
            [] ->
              expression

            clauses ->
              clauses_new =
                List.insert_at(
                  clauses,
                  -1,
                  {:->, [], [[Macro.var(:_, nil)], Macro.var(:body, nil)]}
                )

              function_body_new =
                [
                  quote(
                    do: body = unquote({:case, [], [Macro.var(:body, nil), [do: clauses_new]]})
                  )
                  | function_body
                ]

              {:def, def_metadata,
               [function_header, [do: {:__block__, block_metadata, function_body_new}]]}
          end

        {:@, _attribute_metadata, [{:base_url, _base_url_metadata, _base_url_context}]} =
            base_url_attribute ->
          case operation do
            %Operation{request_path: "/ratenbu.php", request_method: :get} ->
              "https://api.buh.privatbank.ua"

            _ ->
              base_url_attribute
          end

        {:request_url, url} ->
          url
          |> URI.new!()
          |> struct!(query: nil)
          |> URI.to_string()
          |> then(&{:request_url, &1})

        expression ->
          expression
      end)
    end
  end
end
