defmodule Mix.Tasks.Generate do
  @moduledoc "Generates library's modules"
  use Mix.Task
  alias Jason.OrderedObject

  @liqpay_base_url "https://www.liqpay.ua"

  @checkout_webpage_url "#{@liqpay_base_url}/en/doc/api/internet_acquiring/checkout?tab=1"
  @checkout_webpage_file "tmp/checkout.html"
  @checkout_json_file "tmp/checkout.json"

  @cash_webpage_url "https://www.liqpay.ua/en/doc/api/internet_acquiring/cash?tab=1"
  @cash_webpage_file "tmp/cash.html"
  @cash_json_file "tmp/cash.json"

  require Record

  Record.defrecordp(:section,
    node: nil,
    is_request: true,
    update_operation: :new,
    update_type: :object,
    update_name: nil,
    description: nil
  )

  Record.defrecordp(:parse_options,
    main_block_class: nil,
    code_text_class: nil,
    table_class: nil,
    table_standalone_code_block_class: nil,
    standalone_code_block_class: nil,
    section_classes: [],
    section_caption_class: nil
  )

  @requirements ["app.start"]
  @shortdoc "Generates library's modules"
  def run(_) do
    {:ok, _} = Application.ensure_all_started(:wallaby)

    {:ok, session} = Wallaby.start_session()

    process_page(@checkout_webpage_url, session, @checkout_webpage_file, @checkout_json_file)
    process_page(@cash_webpage_url, session, @cash_webpage_file, @cash_json_file)
  end

  defp process_page(url, session, html_file, json_file) do
    with {:ok, body} <- http_request(url, session, html_file),
         [main_block_class] = ~r/new_doc_page_doc__\w+/ |> Regex.run(body, capture: :first),
         [section_caption_class] =
           ~r/new_doc_possibilities_text__\w+/ |> Regex.run(body, capture: :first),
         [code_text_class] =
           ~r/new_doc_integration_code_text__\w+/ |> Regex.run(body, capture: :first),
         [table_class] =
           ~r/new_doc_table_scroll__\w+/ |> Regex.run(body, capture: :first),
         table_standalone_code_block_class =
           ~r/new_doc_table_code__\w+/
           |> Regex.run(body, capture: :first)
           |> (case do
                 [class] -> class
                 nil -> nil
               end),
         [standalone_code_block_class] =
           ~r/new_doc_page_content__\w+/ |> Regex.run(body, capture: :first),
         {:ok, document} <- parse_document(body),
         main_doc =
           document
           |> Floki.find(
             "div.#{main_block_class} > div.MuiBox-root > div.MuiBox-root > div.MuiBox-root"
           )
           |> Enum.find(fn potential_block ->
             not (potential_block
                  |> Floki.find("div.#{table_class}")
                  |> Enum.empty?())
           end),
         {:ok, _spec} <-
           find_spec(
             document,
             parse_options(
               main_block_class: main_block_class,
               code_text_class: code_text_class,
               table_class: table_class,
               table_standalone_code_block_class: table_standalone_code_block_class,
               standalone_code_block_class: standalone_code_block_class,
               section_classes: node_classes(main_doc),
               section_caption_class: section_caption_class
             ),
             json_file
           ) do
    end
  end

  defp http_request(url, session, file) do
    if File.exists?(file) do
      File.read(file)
    else
      session
      |> Wallaby.Browser.visit(url)
      |> Wallaby.Browser.page_source()
      |> tap(&File.write!(file, &1))
      |> then(&{:ok, &1})
    end
  end

  defp parse_document(body) do
    case Floki.parse_document(body) do
      {:ok, document} ->
        {:ok, document}

      {:error, message} ->
        IO.warn("Error parsing HTML document: `#{inspect(message)}`")
        :error
    end
  end

  defp extract_code(code_element) do
    code =
      code_element
      |> Floki.children()
      |> Enum.map_join(fn
        str when is_binary(str) ->
          str

        {"span", _, _} = span ->
          classes = span |> node_classes() |> MapSet.new()

          cond do
            not MapSet.member?(classes, "token") or MapSet.member?(classes, "comment") ->
              ""

            classes
            |> MapSet.intersection(MapSet.new(["property", "string"]))
            |> Enum.empty?() ->
              Floki.text(span)

            :string_or_property ->
              span
              |> Floki.text()
              |> then(&Regex.scan(~r/^(['"])?(.*)(?(1)\1|)$/, &1, capture: :all_but_first))
              |> case do
                [[_quote, string]] -> ~s<"#{string}">
                [[string]] -> ~s<"#{string}">
              end
          end
      end)

    ~r/^\s*(")?(\w+)(?(1)\1|)\s*:(.*)$/s
    |> Regex.scan(code, capture: :all_but_first)
    |> case do
      [["", name, value]] -> ~s<{"#{name}": #{value}}>
      [["\"", name, value]] -> ~s<{"#{name}": #{value}}>
      [] -> code
    end
  end

  defp parse_section(
         section,
         parse_options(section_caption_class: section_caption_class) = parse_options
       ) do
    caption =
      section
      |> Floki.find("div.#{section_caption_class}.MuiBox-root")
      |> hd()
      |> Floki.text()
      |> String.trim()
      |> String.replace(~r/\s+/, " ")
      |> String.trim_trailing(":")

    ~r/^\s*(.*)\s+\(\s*the\s+(\w+)\s+(\w+)\s*\)\s*$/
    |> Regex.scan(caption, capture: :all_but_first)
    |> case do
      [[description, type, name]] ->
        section(
          node: section,
          update_operation: :patch,
          update_type: parse_property_type([type], parse_options),
          update_name: parse_property_name([name], parse_options),
          description: description
        )

      [] ->
        case String.downcase(caption) do
          main
          when main in ["main", "other parameters", "parameters of splitting the payments"] ->
            section(node: section, update_operation: :patch, description: caption)

          "sender parameters" ->
            section(
              node: section,
              update_operation: :new,
              update_name: "sender",
              description: caption
            )

          "regular payment parameters" ->
            section(
              node: section,
              update_operation: :new,
              update_name: "regular-payment",
              description: caption
            )

          "parameters for 1-click payment" ->
            section(
              node: section,
              update_operation: :new,
              update_name: "one-click-payment",
              description: caption
            )

          "response parameters" ->
            section(
              node: section,
              is_request: false,
              update_operation: :patch,
              description: caption
            )
        end
    end
  end

  defp update_section_schema(schema, _section_data, _parse_options, true, _path) do
    {true, schema}
  end

  defp update_section_schema(
         %{type: type} = schema,
         section(update_type: type, update_name: nil) = section_data,
         parse_options,
         false,
         path
       ) do
    schema_new = process_section_properties(schema, section_data, parse_options, path)
    {true, schema_new}
  end

  defp update_section_schema(
         %{type: type} = schema,
         section(update_operation: :patch, update_type: type, update_name: name) = section_data,
         parse_options,
         false,
         [name | _] = path
       ) do
    path_new = if type == :array, do: [[] | path], else: path
    schema_new = process_section_properties(schema, section_data, parse_options, path_new)
    {true, schema_new}
  end

  defp update_section_schema(
         %{type: type} = schema,
         section(update_operation: :new, update_type: type, update_name: name) = section_data,
         parse_options,
         false,
         path
       )
       when is_binary(name) do
    schema_new = process_section_properties(schema, section_data, parse_options, [name | path])
    {true, schema_new}
  end

  defp update_section_schema(
         %{type: :object, properties: properties} = schema,
         section_data,
         parse_options,
         false,
         path
       ) do
    {properties_new, is_processed} =
      Enum.map_reduce(properties, false, fn {key, schema}, is_processed ->
        {is_processed_new, schema_new} =
          update_section_schema(schema, section_data, parse_options, is_processed, [key | path])

        {{key, schema_new}, is_processed_new}
      end)

    schema_new = %{schema | properties: OrderedObject.new(properties_new)}
    {is_processed, schema_new}
  end

  defp update_section_schema(
         %{type: :array, items: items} = schema,
         section_data,
         parse_options,
         false,
         path
       ) do
    {is_processed, items_new} =
      update_section_schema(items, section_data, parse_options, false, [[] | path])

    schema_new = %{schema | items: items_new}
    {is_processed, schema_new}
  end

  defp update_section_schema(
         schema,
         _section_data,
         _parse_options,
         false,
         _path
       ) do
    {false, schema}
  end

  defp process_section_properties(
         schema,
         section(node: node, is_request: is_request) = section_data,
         parse_options(
           table_class: table_class,
           table_standalone_code_block_class: table_standalone_code_block_class
         ) = parse_options,
         path
       ) do
    {properties, required} =
      node
      |> Floki.find(
        "div.#{table_class}.MuiBox-root table.MuiTable-root tbody.MuiTableBody-root tr.MuiTableRow-root"
      )
      |> Enum.map(fn property ->
        [name, required, type, description | rest] =
          property
          |> Floki.find("td.MuiTableCell-root.MuiTableCell-body")
          |> case do
            [name, type, description | rest] when not is_request ->
              [name, ["Optional"], type, description | rest]

            full_property ->
              full_property
          end

        name = parse_property_name(name, parse_options)
        description = parse_property_description(description, parse_options)
        type = parse_property_type(type, parse_options)
        required = parse_property_required(required, parse_options)

        path_new = [name | path]

        property_schema =
          %{type: type, description: description}
          |> initialize_property_processing(path_new)
          |> parse_maximum_length_from_description()
          |> parse_possible_values_from_description()
          |> parse_examples_from_description()
          |> parse_separate_example(rest, path_new)

        {{name, property_schema}, if(required, do: [name], else: [])}
      end)
      |> Enum.unzip()

    properties_new = OrderedObject.new(properties)
    required_new = List.flatten(required)

    properties_new =
      with true <- is_binary(table_standalone_code_block_class),
           [code] <-
             Floki.find(node, "div.#{table_standalone_code_block_class} code.language-json") do
        %{properties: properties_new} =
          code
          |> extract_code()
          |> Jason.decode!()
          |> patch_schema_examples(%{type: :object, properties: properties_new})

        properties_new
      else
        _ -> properties_new
      end

    {properties_new, required_new} =
      case section_data do
        section(update_operation: :new, update_name: name, description: description)
        when is_binary(name) ->
          schema = %{type: :object, properties: properties_new, description: description}

          schema_new =
            if Enum.empty?(required_new) do
              schema
            else
              Map.put(schema, :required, required_new)
            end

          {OrderedObject.new([{name, schema_new}]), []}

        _ ->
          {properties_new, required_new}
      end

    case schema do
      %{type: :object} ->
        schema_new =
          Map.update(schema, :properties, properties_new, fn %OrderedObject{values: values} ->
            OrderedObject.new(values ++ properties_new.values)
          end)

        if Enum.empty?(required_new) do
          schema_new
        else
          Map.update(schema_new, :required, required_new, &(&1 ++ required_new))
        end

      %{type: :array} ->
        items_new =
          schema
          |> Map.fetch(:items)
          |> case do
            {:ok, %{type: :object} = items} -> items
            :error -> %{type: :object}
          end
          |> Map.update(:properties, properties_new, fn %OrderedObject{values: values} ->
            OrderedObject.new(values ++ properties_new.values)
          end)

        items_new =
          if Enum.empty?(required_new) do
            items_new
          else
            Map.update(items_new, :required, required_new, &(&1 ++ required_new))
          end

        Map.put(schema, :items, items_new)
    end
  end

  defp parse_property_name(str, _parse_options) do
    str |> Floki.text() |> String.trim()
  end

  defp parse_property_type(str, _parse_options) do
    str
    |> Floki.text()
    |> String.trim()
    |> String.downcase()
    |> case do
      "string" -> :string
      "number" -> :number
      "array" -> :array
      "object" -> :object
      "boolean" -> :boolean
    end
  end

  defp parse_property_description(
         str,
         parse_options(code_text_class: code_text_class) = parse_options
       ) do
    str
    |> Floki.children()
    |> Enum.map_join(fn
      str when is_binary(str) ->
        result = String.replace(str, ~r/\s+/, " ")

        if result == " " and String.contains?(str, "\n") do
          "\n"
        else
          result
        end

      {"a", _attrs, [text]} = link ->
        [href] = Floki.attribute(link, "href")

        "[#{text |> String.replace(~r/\s+/, " ") |> String.trim()}](#{@liqpay_base_url |> URI.merge(href) |> URI.to_string()})"

      {"br", _attrs, _children} ->
        "\n"

      {"b", _attrs, [text]} = _bold ->
        "**#{text}**"

      {"span", _attrs, [text]} = span ->
        span
        |> node_classes()
        |> Enum.member?(code_text_class)
        |> if(do: "`#{text}`", else: text)

      {"div", _attrs, _children} = div ->
        div
        |> parse_property_description(parse_options)
        |> String.trim_trailing()
        |> Kernel.<>("\n")
    end)
    |> String.replace(~r/\n\s+([^[:upper:]`])/, " \\1")
    |> String.trim()
  end

  defp parse_property_required(str, _parse_options) do
    str
    |> Floki.text()
    |> String.trim()
    |> String.downcase()
    |> case do
      "required" -> true
      "optional" -> false
    end
  end

  defp initialize_property_processing(%{type: :string} = property, ["split_rules"] = path) do
    property
    |> Map.merge(%{
      type: :array,
      items: %{
        type: :object,
        properties:
          OrderedObject.new([
            {
              "public_key",
              %{
                type: :string,
                description: "Public key - the store identifier"
              }
            },
            {"amount",
             %{
               type: :number,
               description: "Payment amount"
             }},
            {"commission_payer",
             %{
               type: :string,
               description: "Commission payer",
               default: "sender",
               enum: ["sender", "receiver"]
             }},
            {"server_url",
             %{
               type: :string,
               format: :uri,
               description:
                 "URL API in your store for notifications of payment status change (`server` -> `server`)",
               maxLength: 510
             }},
            {"description",
             %{
               type: :string,
               description: "Payment description"
             }}
          ]),
        required: [
          "amount"
        ]
      }
    })
    |> initialize_property_processing(path)
  end

  # defp initialize_property_processing(%{type: :string} = property, [boolean_property] = path)
  #      when boolean_property in ~w(verifycode) and
  #             not is_map_key(property, :format) do
  #   property
  #   |> Map.put(:format, "string-yesno")
  #   |> initialize_property_processing(path)
  # end

  defp initialize_property_processing(%{type: :string} = property, [boolean_property] = path)
       when boolean_property in ~w(verifycode) and
              not is_map_key(property, :enum) do
    property
    |> Map.put(:enum, ["Y"])
    |> initialize_property_processing(path)
  end

  # defp initialize_property_processing(%{type: :string} = property, path)
  #      when path in [["subscribe"], ["prepare"], ["recurringbytoken", "one-click-payment"]] and
  #             not is_map_key(property, :format) do
  #   property
  #   |> Map.put(:format, "string-integer")
  #   |> initialize_property_processing(path)
  # end

  defp initialize_property_processing(%{type: :string} = property, path)
       when path in [["subscribe"], ["prepare"], ["recurringbytoken", "one-click-payment"]] and
              not is_map_key(property, :enum) do
    property
    |> Map.put(:enum, ["1"])
    |> initialize_property_processing(path)
  end

  defp initialize_property_processing(%{type: :number} = property, path)
       when path in [["version"], ["id", [], "items", "rro_info"], ["mpi_eci"]] do
    property
    |> Map.put(:type, :integer)
    |> initialize_property_processing(path)
  end

  defp initialize_property_processing(%{type: :string} = property, [url_property] = path)
       when url_property in ~w(result_url server_url product_url) and
              not is_map_key(property, :format) do
    property
    |> Map.put(:format, :uri)
    |> initialize_property_processing(path)
  end

  defp initialize_property_processing(%{type: :string} = property, path)
       when path in [["expired_date"], ["subscribe_date_start", "regular-payment"], ["end_date"]] and
              not is_map_key(property, :format) do
    property
    # |> Map.put(:format, "date-time-liqpay")
    |> Map.put(:format, "date-time")
    |> initialize_property_processing(path)
  end

  defp initialize_property_processing(
         %{type: :array} = property,
         ["delivery_emails", "rro_info"] = path
       )
       when not is_map_key(property, :items) do
    property
    |> Map.put(:items, %{type: :string, format: :email})
    |> initialize_property_processing(path)
  end

  defp initialize_property_processing(property, _path), do: property

  defp find_spec(
         document,
         parse_options(
           main_block_class: main_block_class,
           standalone_code_block_class: standalone_code_block_class,
           section_classes: block_classes
         ) = parse_options,
         json_file
       ) do
    [main_block] = document |> Floki.find("div.#{main_block_class}.MuiBox-root.css-0")

    {request_schema, response_schema} =
      main_block
      |> Floki.find("div.#{Enum.join(block_classes, ".")}")
      |> Enum.reduce({nil, nil}, fn section, {request_schema, response_schema} ->
        section(is_request: is_request, update_operation: update_operation) =
          section_data = parse_section(section, parse_options)

        section_schema = if is_request, do: request_schema, else: response_schema

        section_schema_new =
          (section_schema || %{type: :object, properties: OrderedObject.new([])})
          |> update_section_schema(section_data, parse_options, false, [])
          |> case do
            {true, section_schema_new} -> section_schema_new
            {false, section_schema_new} when update_operation != :patch -> section_schema_new
          end

        if is_request,
          do: {section_schema_new, response_schema},
          else: {request_schema, section_schema_new}
      end)

    request_schema_new =
      document
      |> Floki.find(
        "div.#{main_block_class}.MuiBox-root.css-0 > div.MuiBox-root > div.MuiBox-root.css-0 > div.#{standalone_code_block_class}"
      )
      |> Enum.flat_map(fn code_div ->
        code_div
        |> Floki.find("code.language-json")
        |> case do
          [code] ->
            [extract_code(code)]

          [] ->
            code_div
            |> Floki.find("code.language-javascript")
            |> case do
              [code] ->
                [code_string] =
                  code
                  |> extract_code()
                  |> then(
                    &Regex.scan(
                      ~r/liqpay\.(?:cnb_form\(|api\(\s*\"request\"\s*,)\s*(\{(?:[^}{]+|(?R))*+\})/,
                      &1,
                      capture: :all_but_first
                    )
                  )

                code_string

              [] ->
                []
            end
        end
      end)
      |> Enum.map(&Jason.decode!/1)
      |> Enum.reduce(
        request_schema,
        &patch_schema_examples/2
      )

    response_schema_new = response_schema || %{type: :object}

    %{
      openapi: "3.1.0",
      info: %{
        title: "External API",
        version: "3"
      },
      servers: [
        %{
          url: "https://liqpay.ua"
        }
      ],
      paths: %{
        "/test" => %{
          post: %{
            summary: "Checkout",
            operationId: "checkout",
            requestBody: %{
              content: %{
                "application/json" => %{
                  schema: request_schema_new
                }
              }
            },
            responses: %{
              "200" => %{
                description: "200",
                content: %{
                  "application/json" => %{
                    schema: response_schema_new
                  }
                }
              }
            }
          }
        }
      }
    }
    |> Jason.encode!(pretty: true)
    |> then(&File.write!(json_file, &1))
  end

  defp parse_separate_example(schema, [], _path) do
    schema
  end

  defp parse_separate_example(
         %{description: description} = schema,
         [
           {"code", _, _} = code
         ],
         [name | _] = _path
       ) do
    case name do
      "dae" ->
        description_new =
          "#{description |> String.split("\n") |> hd()}\n\nPossible `JSON` object:\n```\n#{code |> Floki.text() |> String.trim()}\n```"

        %{schema | description: description_new}

      "split_rules" ->
        description_new =
          String.replace(description, ~r/\.?\s*Example\s+(`)?JSON(?(1)\1|)\s+string:?\s*$/, "")

        code
        |> extract_code()
        |> Jason.decode!()
        |> patch_schema_examples(%{schema | description: description_new})
    end
  end

  defp parse_separate_example(schema, [{_, _, _} = node], path) do
    parse_separate_example(schema, Floki.find(node, "code.language-json"), path)
  end

  defp patch_schema_examples(example, %{type: :object, properties: properties} = schema)
       when is_map(example) do
    properties_new =
      Enum.reduce(
        example,
        properties,
        fn {key, value}, properties ->
          update_in(properties, [key], &patch_schema_examples(value, &1))
        end
      )

    %{schema | properties: properties_new}
  end

  defp patch_schema_examples(example, %{type: :array, items: items} = schema)
       when is_list(example) do
    items_new =
      Enum.reduce(
        example,
        items,
        &patch_schema_examples(&1, &2)
      )

    %{schema | items: items_new}
  end

  defp patch_schema_examples(example, schema) do
    example_new = parse_schema_value(example, schema)
    Map.update(schema, :examples, [example_new], &Enum.uniq(&1 ++ [example_new]))
  end

  defp parse_maximum_length_from_description(%{description: description} = options) do
    ~r/(?:\.\s+)?(?:The\s+m|M)ax(?:imum)?\s+length(?:\s+is)?\s+(\*\*)?(\d+)\1?\s+symbols/
    |> Regex.scan(description)
    |> case do
      [[full_match, max_length]] ->
        description_new = String.replace(description, full_match, "")

        Map.merge(options, %{
          maxLength: String.to_integer(max_length),
          description: description_new
        })

      [[full_match, "**", max_length]] ->
        description_new = String.replace(description, full_match, "")

        Map.merge(options, %{
          maxLength: String.to_integer(max_length),
          description: description_new
        })

      [] ->
        options
    end
  end

  defp parse_possible_values_from_description(%{description: description} = options) do
    ~r/(\.\s+)?((?:Possible|Present)\s+values?\s*:?|Current\s+value\s*\-?|^Customer's\s+language)\n?([^\.\n]+)(?:\.|$)/
    |> Regex.scan(description)
    |> case do
      [[full_match, prefix, prefix_text, values_match]] ->
        {enum_options, has_descriptions} =
          ~r/\s*`([^`]+?)`(?:\s+\-\s+([^,\.\n`]+))?[\n,]?/
          |> Regex.scan(values_match, capture: :all_but_first)
          |> Enum.map_reduce(false, fn
            [key], has_descriptions -> {{key, nil}, has_descriptions}
            [key, description], _has_descriptions -> {{key, description}, true}
          end)

        {full_match_new, prefix_new} =
          if prefix_text == "Customer's language" do
            {values_match, "#{prefix}\n"}
          else
            {full_match, prefix}
          end

        description_new =
          if has_descriptions do
            enum_options
            |> Enum.map_join(
              "\n",
              fn
                {key, nil} -> "* `#{key}`"
                {key, description} -> "* `#{key}` - #{description}"
              end
            )
            |> then(
              &String.replace(description, full_match_new, "#{prefix_new}Possible values:\n#{&1}")
            )
          else
            description
            |> String.replace(full_match_new, "")
            |> String.replace(~r/^\s*\.\s*/, "")
          end

        enum =
          enum_options
          |> Enum.map(fn {key, _} ->
            parse_schema_value(key, options)
          end)
          |> Enum.uniq()

        options_new =
          Map.merge(options, %{
            enum: enum,
            description: description_new
          })

        if not has_descriptions and length(enum_options) == 1 and
             prefix_text |> String.replace(~r/\s+/, " ") |> String.starts_with?("Current value") do
          [{default, _}] = enum_options
          default_new = parse_schema_value(default, options)
          Map.put(options_new, :default, default_new)
        else
          options_new
        end

      [] ->
        ~r/^((?:\s*`[^`]+?`,?)+)$/
        |> Regex.match?(description)
        |> if do
          enum =
            ~r/`([^`]+?)`/
            |> Regex.scan(description, capture: :all_but_first)
            |> Enum.flat_map(fn [str] -> String.split(str, ",") end)
            |> Enum.map(&String.trim/1)

          options
          |> Map.update(:enum, enum, &Enum.uniq(&1 ++ enum))
          |> Map.delete(:description)
        else
          ~r/^\s*`([^`]+)`\s*-\s*(.+)$/m
          |> Regex.scan(description)
          |> Enum.reduce(options, fn [full_match, key, key_description],
                                     %{description: description} = options ->
            description_new =
              String.replace(description, full_match, "* `#{key}` - #{key_description}")

            key_new = parse_schema_value(key, options)

            options
            |> Map.update(:enum, [key_new], &Enum.uniq(&1 ++ [key_new]))
            |> Map.put(:description, description_new)
          end)
        end
    end
  end

  defp parse_examples_from_description(%{description: description} = options) do
    ~r/(?:\.\s+)?(?:For\s+example):?((?:\s*`[^`]+?`,?)+)(?:\.|$)/
    |> Regex.scan(description)
    |> case do
      [[full_match, examples_match]] ->
        process_examples_match_in_description(options, full_match, examples_match)

      [] ->
        options
    end
  end

  defp parse_examples_from_description(options), do: options

  defp process_examples_match_in_description(
         %{description: description} = options,
         full_match,
         match
       ) do
    examples =
      ~r/`([^`]+?)`/
      |> Regex.scan(match, capture: :all_but_first)
      |> Enum.map(fn [example] ->
        ~r/^«(.+)»$/s
        |> Regex.scan(example, capture: :all_but_first)
        |> case do
          [[stripped_example]] -> stripped_example
          [] -> example
        end
        |> parse_schema_value(options)
      end)
      |> Enum.uniq()

    description_new =
      description
      |> String.replace(full_match, "")
      |> String.trim()

    Map.merge(options, %{examples: examples, description: description_new})
  end

  defp node_classes(node) do
    node
    |> Floki.attribute("class")
    |> Enum.flat_map(&String.split/1)
    |> Enum.uniq()
  end

  defp parse_schema_value(value, %{type: type} = _schema) do
    {:ok, value_decoded} =
      OpenAPIClient.Client.TypedDecoder.decode(
        value,
        type,
        [],
        OpenAPIClient.Client.TypedDecoder
      )

    value_decoded
  end
end
