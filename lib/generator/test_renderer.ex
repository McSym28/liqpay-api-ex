if Mix.env() in [:dev] do
  defmodule LiqPayAPI.Generator.TestRenderer do
    use OpenAPIClient.Generator.TestRenderer
    alias OpenAPI.Processor.{Operation, Schema}
    alias OpenAPIClient.Generator.Schema, as: GeneratorSchema
    alias OpenAPIClient.Generator.Field, as: GeneratorField
    alias Schema.Field

    @impl OpenAPIClient.Generator.TestRenderer
    def example(_state, {:string, "date-time-liqpay"}, _path), do: "2024-01-02 01:23:45"
    def example(_state, {:string, "date-liqpay"}, _path), do: "01.02.2024"
    def example(_state, {:string, "month-year-liqpay"}, _path), do: "0124"
    def example(_state, {:integer, "timestamp-ms"}, _path), do: 1_706_750_625_987
    def example(_state, {:string, "boolean-integer"}, _path), do: "1"
    def example(_state, {:string, "boolean-yesno"}, _path), do: "Y"

    Enum.map(
      LiqPayAPI.Client.TypedEncoder.nested_clauses(),
      fn {module, type, nested_fields} ->
        def example(
              state,
              %GeneratorSchema{
                schema: %Schema{module_name: unquote(module), type_name: unquote(type)},
                fields: fields
              } = generator_schema,
              path
            ) do
          nested_keys =
            Enum.flat_map(fields, fn
              %GeneratorField{field: %Field{name: name}, old_name: old_name}
              when name in unquote(Enum.map(nested_fields, &Atom.to_string/1)) ->
                [old_name]

              _ ->
                []
            end)

          {nested_fields, map_rest} =
            state
            |> OpenAPIClient.Generator.TestRenderer.example(generator_schema, path)
            |> Map.split(nested_keys)

          Enum.reduce(
            nested_fields,
            map_rest,
            fn {_key, value}, acc -> Map.merge(acc, value) end
          )
        end
      end
    )

    def example(state, type, path),
      do: OpenAPIClient.Generator.TestRenderer.example(state, type, path)

    @impl OpenAPIClient.Generator.TestRenderer
    Enum.map(
      LiqPayAPI.Client.TypedEncoder.nested_clauses(),
      fn {module, type, nested_fields} ->
        def decode_example(
              state,
              value,
              %GeneratorSchema{
                schema: %Schema{module_name: unquote(module), type_name: unquote(type)},
                fields: fields
              } = generator_schema,
              path
            ) do
          value_new =
            fields
            |> Enum.filter(fn
              %GeneratorField{field: %Field{name: name}}
              when name in unquote(Enum.map(nested_fields, &Atom.to_string/1)) ->
                true

              _ ->
                false
            end)
            |> Enum.reduce(value, fn
              %GeneratorField{field: %Field{type: schema_ref}, old_name: old_name}, acc ->
                [{_, %GeneratorSchema{fields: nested_fields} = _field_schema}] =
                  :ets.lookup(:schemas, schema_ref)

                nested_keys =
                  Enum.flat_map(nested_fields, fn
                    %GeneratorField{old_name: old_name} -> [old_name]
                    _ -> []
                  end)

                {nested_fields, acc_rest} = Map.split(acc, nested_keys)
                Map.put(acc_rest, old_name, nested_fields)
            end)

          OpenAPIClient.Generator.TestRenderer.decode_example(
            state,
            value_new,
            generator_schema,
            path
          )
        end
      end
    )

    def decode_example(state, value, type, path),
      do: OpenAPIClient.Generator.TestRenderer.decode_example(state, value, type, path)

    @impl OpenAPIClient.Generator.TestRenderer
    def render_operation_test(state, operation, request_schema, response_schema) do
      {macro, _private_key} =
        state
        |> OpenAPIClient.Generator.TestRenderer.render_operation_test(
          operation,
          request_schema,
          response_schema
        )
        |> Macro.prewalk(nil, fn
          {:expect, expect_metadata,
           [
             {:@, _attribute_metadata, [{:httpoison, _httpoison_metadata, _httpoison_context}]} =
                 httpoison_attribute,
             :request,
             {:fn, fn_metadata,
              [
                {:->, pipe_metadata,
                 [
                   [method, url | fn_arguments_rest] = _fn_arguments,
                   {:__block__, fn_block_metadata, fn_block_expressions}
                 ]}
              ]}
           ]},
          private_key ->
            url_new =
              url
              |> URI.new!()
              |> struct!(query: nil)
              |> URI.to_string()

            fn_arguments_new = [method, url_new | fn_arguments_rest]

            fn_block_expressions_new =
              case operation do
                %Operation{request_path: <<"/api/request", _rest::binary>>, request_method: :post} ->
                  [
                    quote(
                      do:
                        assert(
                          {:ok, "application/x-www-form-urlencoded"} ==
                            with {_, content_type_request} <-
                                   List.keyfind(headers, "content-type", 0),
                                 {:ok, {media_type, media_subtype, _parameters}} =
                                   OpenAPIClient.Client.Operation.parse_content_type_header(
                                     content_type_request
                                   ) do
                              {:ok, "#{media_type}/#{media_subtype}"}
                            end
                        )
                    ),
                    quote(do: form_data = URI.decode_query(body)),
                    quote(do: assert({:ok, signature} = Map.fetch(form_data, "signature"))),
                    quote(do: assert({:ok, data} = Map.fetch(form_data, "data"))),
                    quote(
                      do:
                        assert(
                          LiqPayAPI.Client.Signature.check?(data, unquote(private_key), signature)
                        )
                    ),
                    quote(do: assert({:ok, body} = Base.decode64(data))),
                    quote(
                      do:
                        headers =
                          List.keystore(
                            headers,
                            "content-type",
                            0,
                            {"content-type", "application/json"}
                          )
                    )
                    | fn_block_expressions
                  ]

                _ ->
                  fn_block_expressions
              end

            expression_new =
              {:expect, expect_metadata,
               [
                 httpoison_attribute,
                 :request,
                 {:fn, fn_metadata,
                  [
                    {:->, pipe_metadata,
                     [
                       fn_arguments_new,
                       {:__block__, fn_block_metadata, fn_block_expressions_new}
                     ]}
                  ]}
               ]}

            {expression_new, private_key}

          {:assert, _,
           [
             {:=, _,
              [
                {{:_, _, _}, private_key},
                {{:., _, [{:__aliases__, _, [:List]}, :keyfind]}, _,
                 [{:params, _, _}, :private_key, 0]}
              ]}
           ]} = expression,
          nil ->
            {expression, private_key}

          expression, private_key ->
            {expression, private_key}
        end)

      macro
    end
  end
end
