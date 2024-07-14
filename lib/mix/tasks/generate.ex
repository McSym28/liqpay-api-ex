defmodule Mix.Tasks.Generate do
  @moduledoc "Generates library's modules"
  use Mix.Task
  # use Wallaby.Feature
  use Wallaby.DSL

  @liqpay_base_url "https://www.liqpay.ua"

  @checkout_webpage_url "#{@liqpay_base_url}/en/doc/api/internet_acquiring/checkout?tab=1"
  @checkout_webpage_file "tmp/checkout.html"
  # # @checkout_webpage_file "tmp/liqpay/acquiring_applepay.html"

  # @subscription_webpage_file "tmp/liqpay/subscription2.html"
  # @subscription_webpage_url "https://www.liqpay.ua/en/doc/api/internet_acquiring/subscription?tab=1"

  @requirements ["app.start"]
  @shortdoc "Generates library's modules"
  def run(_) do
    {:ok, _} = Application.ensure_all_started(:wallaby)

    {:ok, session} = Wallaby.start_session()

    with {:ok, body} <- http_request(@checkout_webpage_url, session, @checkout_webpage_file),
         [main_block_class] = ~r/new_doc_page_doc__\w+/ |> Regex.run(body, capture: :first),
         [block_title_class] =
           ~r/new_doc_possibilities_text__\w+/ |> Regex.run(body, capture: :first),
         {:ok, document} <- parse_document(body),
         [standalone_code_block_class] =
           ~r/new_doc_page_content__\w+/ |> Regex.run(body, capture: :first),
         #  [main_block] = document |> Floki.find("div.#{main_block_class}.MuiBox-root.css-0"),
         [main_doc] = document |> Floki.find("div.#main_doc"),
         block_classes = node_classes(main_doc),
         #  document |> Floki.find("div.MuiBox-root:has(> div.#{block_title_class})") |> Enum.map(&node_classes/1) |> IO.inspect(label: "blocks.classes"),
         #  document |> Floki.find("div.#{doc_block_class}.MuiBox-root.css-0 > div.MuiBox-root.css-0  > div.MuiBox-root.css-0  > div.MuiBox-root") |> Enum.map(&node_classes/1) |> IO.inspect(label: "blocks.classes"),
         {:ok, _spec} <-
           find_spec(
             document,
             main_block_class,
             standalone_code_block_class,
             block_classes,
             block_title_class
           ) do
      # spec_filepath =
      #   @temp_spec_filename_length
      #   |> :crypto.strong_rand_bytes()
      #   |> Base.url_encode64(padding: false)
      #   |> then(&Enum.join([&1, "json"], "."))
      #   |> then(&Path.join(tmp_dir, &1))

      # if File.exists?(spec_filepath) do
      #   File.rm!(spec_filepath)
      # end

      # File.write!("test/fixtures/mono.json",  Jason.encode!(spec, pretty: true))

      # spec
      # |> traverse_spec([])
      # |> Jason.encode!(pretty: true)
      # |> then(&File.write!(spec_filepath, &1))

      # File.rm_rf!("lib/acquiring/*")
      # File.rm_rf!("test/acquiring/*")

      # Mix.Task.run("api.gen", ["acquiring", spec_filepath])

      # File.rm!(spec_filepath)
    end

    # HTTPoison.start()

    # # tmp_dir = System.tmp_dir!()

    # with {:ok, body} <- http_request(@checkout_url),
    #      {:ok, document} <- parse_document(body),
    #      {:ok, _spec} <- find_spec(document) do
    #   # spec_filepath =
    #   #   @temp_spec_filename_length
    #   #   |> :crypto.strong_rand_bytes()
    #   #   |> Base.url_encode64(padding: false)
    #   #   |> then(&Enum.join([&1, "json"], "."))
    #   #   |> then(&Path.join(tmp_dir, &1))

    #   # if File.exists?(spec_filepath) do
    #   #   File.rm!(spec_filepath)
    #   # end

    #   # File.write!("test/fixtures/mono.json",  Jason.encode!(spec, pretty: true))

    #   # spec
    #   # |> traverse_spec([])
    #   # |> Jason.encode!(pretty: true)
    #   # |> then(&File.write!(spec_filepath, &1))

    #   # File.rm_rf!("lib/acquiring/*")
    #   # File.rm_rf!("test/acquiring/*")

    #   # Mix.Task.run("api.gen", ["acquiring", spec_filepath])

    #   # File.rm!(spec_filepath)
    # end
  end

  defp http_request(url, session, file) do
    if File.exists?(file) do
      File.read(file)
    else
      session
      |> visit(url)
      |> page_source()
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
            not MapSet.member?(classes, "token") ->
              ""

            MapSet.member?(classes, "comment") ->
              ""

            classes
            |> MapSet.intersection(MapSet.new(["property", "string", "string-property"]))
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

    # IO.puts(code)
    ~r/^\s*(")?(\w+)(?(1)\1|)\s*:(.*)$/
    |> Regex.scan(code, capture: :all_but_first)
    |> case do
      [["", name, value]] -> ~s<{"#{name}": #{value}}>
      [["\"", name, value]] -> ~s<{"#{name}": #{value}}>
      [] -> code
    end
  end

  defp find_spec(
         document,
         main_block_class,
         standalone_code_block_class,
         block_classes,
         block_title_class
       ) do
    # document
    # |> Floki.find("code.language-javascript")
    # |> Enum.map(fn {"code", _, _} = code ->
    #   [[json]] =
    #     code
    #     |> extract_code()
    #     |> then(
    #       &Regex.scan(
    #         ~r/liqpay\.(?:cnb_form\(|api\(\s*\"request\"\s*,)\s*(\{(?:[^}{]+|(?R))*+\})/,
    #         &1,
    #         capture: :all_but_first
    #       )
    #     )

    #   # IO.inspect(json, label: "language-javascript")
    #   json |> Jason.decode!() |> IO.inspect(pretty: true)
    # end)

    # document
    # |> Floki.find("code.language-json")
    # |> Enum.map(fn {"code", _, _} = code ->
    #   code
    #   |> extract_code()
    #   # |> IO.inspect(label: "language-json")
    #   # |> IO.puts()
    #   |> Jason.decode!() |> IO.inspect(pretty: true)
    # end)

    # IO.puts(standalone_code_block_class)

    [main_block] = document |> Floki.find("div.#{main_block_class}.MuiBox-root.css-0")

    sections =
      main_block
      # |> Floki.find("div.new_doc_possibilities_text__MLKp5.MuiBox-root")
      # |> Floki.find("div.MuiBox-root.css-14kxyr")
      # |> Floki.find("div.MuiBox-root.css-0 > div.MuiBox-root.css-0 > div.MuiBox-root")
      |> Floki.find("div.#{Enum.join(block_classes, ".")}")
      # |> Enum.each(&IO.inspect(&1))
      # |> Enum.flat_map(
      # |> Map.new(
      |> Enum.flat_map(fn section ->
        caption =
          section
          |> Floki.find("div.#{block_title_class}.MuiBox-root")
          |> Floki.text()
          |> String.replace(~r/\s+/, " ")
          |> String.trim_trailing(":")

        # if caption == "" or caption |> String.downcase() |> String.contains?("response") do
        #   []
        # else

        # IO.inspect(caption, label: "caption")

        # if caption == "Main" do
        properties =
          section
          |> Floki.find(
            "div.new_doc_table_scroll__tz8vk.MuiBox-root  table.MuiTable-root tbody.MuiTableBody-root tr.MuiTableRow-root"
          )
          # |> Enum.map(
          |> Enum.map(fn parameter ->
            # IO.inspect(parameter, label: caption)
            [name, required, type, description | rest] =
              Floki.find(parameter, "td.MuiTableCell-root.MuiTableCell-body")

            name = name |> Floki.text() |> String.trim()

            description =
              description
              |> Floki.children()
              # &IO.inspect(&1, label: "#{caption}/#{name}")
              |> Enum.map_join(fn
                str when is_binary(str) ->
                  # if Regex.match?(~r/^\s+$/, str), do: " ", else: String.trim(str)
                  String.replace(str, ~r/\s+/, " ")

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
                  |> Enum.member?("new_doc_integration_code_text__G93bP")
                  |> if(do: "`#{text}`", else: text)

                  # span
                  # |>
                  # |> case do
                  #   [classes] ->

                  #     |> if do

                  #     else
                  #       text
                  #     end
                  #     end
                  # # |> then(&IO.inspect(&1, label: "#{caption}/#{name}"))
                  # # text
              end)
              |> String.replace(~r/\n\s+([^[:upper:]])/, " \\1")
              |> String.trim()

            type =
              type
              |> Floki.text()
              |> String.trim()
              |> String.downcase()
              |> case do
                "string" -> :string
                "number" -> :number
                "integer" -> :integer
                "array" -> :array
                "object" -> :object
                "boolean" -> :boolean
              end

            required =
              required
              |> Floki.text()
              |> String.trim()
              |> String.downcase()
              |> case do
                "required" -> true
                "optional" -> false
              end

            options =
              %{type: type, required: required, description: description}
              |> parse_maximum_length_from_description()

            {description_new, options_new} =
              parse_possible_values_from_description(description_new, options_new)

            {description_new, options_new} =
              parse_examples_from_description(description_new, name, options_new)

            description_new =
              with [example] <- rest,
                   [code] <- Floki.find(example, "code.language-json"),
                   true <- String.contains?(description, "base64") do
                # if Regex.match?(~r/(`)?JSON(?(1)\1|)\s+string/, description) and
                #      name not in ~w(split_rules) do
                json =
                  code
                  |> extract_code()
                  |> Jason.decode!()

                # |> IO.inspect(label: "language-json")
                # |> IO.inspect(label: name, pretty: true)
                description_new
                |> String.replace(
                  ~r/(?:\.\s+)?(`)?JSON(?(1)\1|)\s+can\s+contain\s+the\s+parameters\s+shown\s+in\s+the\s+example/,
                  ""
                )
                |> Kernel.<>("\n\n`JSON` example:\n```#{Jason.encode!(json, pretty: true)}\n```")

                # end
              else
                _ -> description_new
              end

            # description = Floki.text(description)
            # IO.puts(
            #   "#{caption}: name=#{name}, description=#{description_new}, options=#{inspect(options_new)}"
            # )
            # {{name, Map.put(options_new, :description, description_new)}, if(required, do: [name], else: [])}
            options_new =
              if description_new == "" do
                options_new
              else
                Map.put(options_new, :description, description_new)
              end

            {name, options_new}
          end)

        # end
        [{caption, properties}]
        # end
      end)

    # |> IO.inspect(pretty: true, limit: :infinity)
    # |> Enum.unzip()

    {reference_sections, properties} =
      sections
      |> Enum.map(fn {caption, properties} = _section ->
        ~r/^.*\(the\s+(\w+)\s+(\w+)\)$/
        |> Regex.scan(caption, capture: :all_but_first)
        # |> IO.inspect(label: caption)
        |> case do
          [[type, name]] ->
            {{String.to_existing_atom(type), name}, properties}

          [] ->
            caption = Macro.underscore(caption)

            Enum.map(properties, fn {key, property} ->
              {key, Map.put(property, :block, caption)}
            end)
        end
      end)
      |> Enum.split_with(fn
        {{_type, _name}, _properties} -> true
        _properties -> false
      end)

    properties_new =
      reference_sections
      |> Enum.reduce(
        List.flatten(properties),
        fn reference_section, properties ->
          # IO.inspect(reference_section)
          {properties_new, true} = patch_properties(properties, reference_section, false)
          properties_new
        end
      )

    # standalone_examples =
    schema =
      document
      |> Floki.find(
        "div.#{main_block_class}.MuiBox-root.css-0 > div.MuiBox-root.css-0 > div.MuiBox-root.css-0 > div.#{standalone_code_block_class}"
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
      # |> IO.inspect(label: "examples")
      |> Enum.reduce(
        %{type: :object, properties: properties_new},
        &patch_schema_examples/2
      )
      |> process_property_spec([])

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
                  schema: schema
                }
              }
            },
            responses: %{
              "200" => %{
                description: "200",
                content: %{
                  "application/json" => %{
                    schema: %{
                      type: :object
                    }
                  }
                }
              }
            }
          }
        }
      }
    }
    |> Jason.encode!(pretty: true)
    # |> IO.puts()
    |> then(&File.write!("tmp/checkout.json", &1))
  end

  defp patch_schema_examples(example, %{type: :object, properties: properties} = schema)
       when is_map(example) do
    properties_new =
      Enum.reduce(
        example,
        properties,
        fn {key, value}, properties ->
          {_, property} = List.keyfind!(properties, key, 0)
          property_new = patch_schema_examples(value, property)
          List.keyreplace(properties, key, 0, {key, property_new})
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

  # defp patch_schema_examples(example, %{type: type} = schema) when type in ~w(string number integer boolean)a and (is_binary(example) or is_number(example) or is_boolean(example)) do
  defp patch_schema_examples(example, schema) do
    Map.update(schema, :examples, [example], &[example | &1])
  end

  # defp process_property_spec(%{type: :string} = property, [boolean_property] = path)
  #      when boolean_property in ~w(verifycode) do
  #   property_new = Map.merge(property, %{type: :boolean, format: "string-yesno"})
  #   process_property_spec(property_new, path)
  # end

  defp process_property_spec(%{type: :number} = property, [integer_property] = path)
       when integer_property in ~w(version) do
    property_new = %{property | type: :integer}
    process_property_spec(property_new, path)
  end

  defp process_property_spec(%{type: :number} = property, ["id", [], "items", "rro_info"] = path) do
    property_new = %{property | type: :integer}
    process_property_spec(property_new, path)
  end

  defp process_property_spec(%{type: :string} = property, [url_property] = path)
       when url_property in ~w(result_url server_url) and not is_map_key(property, :format) do
    property_new = Map.put(property, :format, :uri)
    process_property_spec(property_new, path)
  end

  defp process_property_spec(%{type: :string} = property, [datetime_property] = path)
       when datetime_property in ~w(subscribe_date_start expired_date) and
              not is_map_key(property, :format) do
    property_new = Map.put(property, :format, "date-time-liqpay")
    process_property_spec(property_new, path)
  end

  defp process_property_spec(%{type: :array} = property, ["delivery_emails", "rro_info"] = path)
       when not is_map_key(property, :items) do
    IO.inspect(property)
    property_new = Map.put(property, :items, %{type: :string, format: :email})
    process_property_spec(property_new, path)
  end

  defp process_property_spec(%{type: :object, properties: properties} = property, path)
       when not is_map_key(property, :processed) do
    {properties_new, required} =
      properties
      |> Enum.map(fn {name, property} ->
        property_new = process_property_spec(property, [name | path])
        required = property[:required] && (path != [] || property[:block] == "main")
        {{name, property_new}, if(required, do: [name], else: [])}
      end)
      |> Enum.unzip()

    property_new =
      Map.merge(property, %{
        properties: Map.new(properties_new),
        required: List.flatten(required),
        processed: true
      })

    process_property_spec(property_new, path)
  end

  defp process_property_spec(%{type: :array, items: items} = property, path)
       when not is_map_key(property, :processed) do
    items_new = process_property_spec(items, [[] | path])
    property_new = Map.merge(property, %{items: items_new, processed: true})
    process_property_spec(property_new, path)
  end

  defp process_property_spec(%{type: :string, max_length: max_length} = property, path)
       when not is_map_key(property, :maxLength) do
    property_new = Map.put(property, :maxLength, max_length)
    process_property_spec(property_new, path)
  end

  defp process_property_spec(%{type: type} = property, _path) do
    if type in ~w(integer boolean number)a do
      property
      |> Map.replace_lazy(
        :enum,
        &Enum.map(&1, fn value ->
          {:ok, value_decoded} =
            OpenAPIClient.Client.TypedDecoder.decode(
              value,
              type,
              [],
              OpenAPIClient.Client.TypedDecoder
            )

          value_decoded
        end)
      )
      |> Map.replace_lazy(
        :examples,
        &(&1
          |> Enum.map(fn value ->
            {:ok, value_decoded} =
              OpenAPIClient.Client.TypedDecoder.decode(
                value,
                type,
                [],
                OpenAPIClient.Client.TypedDecoder
              )

            value_decoded
          end)
          |> Enum.uniq())
      )
      |> Map.replace_lazy(:default, fn value ->
        {:ok, value_decoded} =
          OpenAPIClient.Client.TypedDecoder.decode(
            value,
            type,
            [],
            OpenAPIClient.Client.TypedDecoder
          )

        value_decoded
      end)
    else
      property
    end
    |> Map.take(~w(type enum default format properties items maxLength description examples)a)
  end

  defp patch_properties(item, _reference_section, true), do: {item, true}

  defp patch_properties(
         {property_name, %{type: :array} = property},
         {{:array, property_name}, reference_properties},
         _is_found
       ) do
    property_new =
      Map.update(
        property,
        :items,
        %{type: :object, properties: reference_properties},
        fn property -> update_in(property, [:properties], &(&1 ++ reference_properties)) end
      )

    {{property_name, property_new}, true}
  end

  defp patch_properties(
         {property_name, %{type: :object} = property},
         {{:object, property_name}, reference_properties},
         _is_found
       ) do
    property_new =
      Map.update(property, :properties, reference_properties, &(&1 ++ reference_properties))

    {{property_name, property_new}, true}
  end

  defp patch_properties(
         {property_name, %{type: :array, items: items} = property},
         reference_section,
         is_found
       ) do
    {items_new, is_found_new} = patch_properties(items, reference_section, is_found)
    # Enum.map_reduce(
    #   items,
    #   is_found,
    #   fn parameter, is_found -> patch_parameters(parameter, reference_section, is_found) end)
    property_new = %{property | items: items_new}
    {{property_name, property_new}, is_found_new}
  end

  defp patch_properties(
         {property_name, %{type: :object, properties: properties} = property},
         reference_section,
         is_found
       ) do
    {properties_new, is_found_new} = patch_properties(properties, reference_section, is_found)
    property_new = %{property | properties: properties_new}
    {{property_name, property_new}, is_found_new}
  end

  defp patch_properties(properties, reference_section, is_found) when is_list(properties) do
    Enum.map_reduce(
      properties,
      is_found,
      fn property, is_found -> patch_properties(property, reference_section, is_found) end
    )
  end

  defp patch_properties(item, _reference_section, is_found), do: {item, is_found}

  defp parse_maximum_length_from_description(%{description: description} = options) do
    ~r/(?:\.\s+)?(?:The\s+m|M)ax(?:imum)?\s+length(?:\s+is)?\s+(\*\*)?(\d+)\1?\s+symbols/
    |> Regex.scan(description)
    |> case do
      [[full_match, max_length]] ->
        description_new = String.replace(description, full_match, "")

        Map.merge(options, %{
          max_length: String.to_integer(max_length),
          description: description_new
        })

      [[full_match, "**", max_length]] ->
        description_new = String.replace(description, full_match, "")

        Map.merge(options, %{
          max_length: String.to_integer(max_length),
          description: description_new
        })

      [] ->
        options
    end
  end

  defp parse_possible_values_from_description(description, options) do
    ~r/(\.\s+)?(Possible\s+values?\s*:?|Current\s+value\s*\-?)([^\.\n]+)(?:\.|$)/
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
            |> then(&String.replace(description, full_match, "#{prefix}Possible values:\n#{&1}"))
          else
            description
            |> String.replace(full_match, "")
            |> String.replace(~r/^\s*\.\s*/, "")
          end

        options_new = Map.put(options, :enum, Enum.map(enum_options, fn {key, _} -> key end))

        options_new =
          if not has_descriptions and length(enum_options) == 1 and
               prefix_text |> String.replace(~r/\s+/, " ") |> String.starts_with?("Current value") do
            [{default, _}] = enum_options
            Map.put(options_new, :default, default)
          else
            options_new
          end

        {description_new, options_new}

      [] ->
        {description, options}
    end
  end

  defp parse_examples_from_description(description, name, options) do
    ~r/(?:\.\s+)?(?:For\s+example|Example\s+of\s+parameter\s+`#{name}`):?((?:\s*(`)?[^`]+?(?(2)\2|),?)+)(?:\.|$)/
    |> Regex.scan(description)
    |> case do
      [[full_match, examples_match, "`"]] ->
        process_examples_match_in_description(description, full_match, examples_match, options)

      [[full_match, examples_match]] ->
        # examples =
        #   ~r/`([^`]+?)`/
        #   |> Regex.scan(examples_match)
        #   |> Enum.map(fn [_example_full_match, example] -> example |> String.trim_leading("«") |> String.trim_trailing("»") end)

        # description_new = String.replace(description, full_match, "")
        # options_new = Map.put(options, :examples, examples)
        # {description_new, options_new}
        process_examples_match_in_description(
          description,
          full_match,
          "`#{String.trim(examples_match)}`",
          options
        )

      [] ->
        ~r/(?<!following\sformat|following\sformat )((?:\s*`[^`]+`,?)+)$/
        |> Regex.scan(description)
        |> case do
          [[full_match, examples_match]] ->
            process_examples_match_in_description(
              description,
              full_match,
              examples_match,
              options
            )

          [] ->
            {description, options}
        end
    end
  end

  defp process_examples_match_in_description(description, full_match, match, options) do
    examples =
      ~r/`([^`]+?)`/
      # ~r/(`)?([^`]+?)(?(1)\1|)/
      |> Regex.scan(match, capture: :all_but_first)
      |> Enum.map(fn [example] ->
        ~r/^«(.+)»$/s
        |> Regex.scan(example, capture: :all_but_first)
        |> case do
          [[stripped_example]] -> stripped_example
          [] -> example
        end
      end)

    description_new =
      description
      |> String.replace(full_match, "")
      |> String.trim()

    options_new = Map.put(options, :examples, examples)
    {description_new, options_new}
  end

  defp node_classes(node) do
    node
    |> Floki.attribute("class")
    |> Enum.flat_map(&String.split/1)
    |> Enum.uniq()
  end

  # defguardp parameter_not_required?(map)
  #           when not is_map_key(map, "required") or :erlang.map_get("required", map) == false

  # # Update `tags` and `operationId` to operations
  # Enum.map(@operation_changes, fn {url, method, changes} ->
  #   defp traverse_spec(
  #          operation_spec,
  #          [unquote(method), unquote(url), "paths"] = path
  #        )
  #        when is_map(operation_spec) do
  #     operation_spec
  #     |> do_traverse_spec(path)
  #     |> Map.merge(%{unquote_splicing(Map.to_list(changes))})
  #   end
  # end)

  # # Fix incorrect examples
  # defp traverse_spec(
  #        {"example", value},
  #        [
  #          "example",
  #          "cvv",
  #          "properties",
  #          "cardData",
  #          "properties",
  #          "PaymentDirectRequest",
  #          "schemas",
  #          "components"
  #        ] = _path
  #      )
  #      when not is_binary(value) do
  #   {"example", to_string(value)}
  # end

  # defp traverse_spec(
  #        {"example", value},
  #        [
  #          "example",
  #          "sst",
  #          "properties",
  #          "cardData",
  #          "properties",
  #          "InvoiceSyncPaymentRequest",
  #          "schemas",
  #          "components"
  #        ] = _path
  #      )
  #      when not is_number(value) do
  #   {float_value, ""} = Float.parse(value)
  #   {"example", float_value}
  # end

  # # Force required params
  # Enum.map(@forced_required_params, fn {url, method, name, location} ->
  #   defp traverse_spec(
  #          %{"name" => unquote(name), "in" => unquote(location)} = value,
  #          [[_index], "parameters", unquote(method), unquote(url), "paths"] = path
  #        )
  #        when parameter_not_required?(value) do
  #     value
  #     |> do_traverse_spec(path)
  #     |> Map.put("required", true)
  #   end
  # end)

  # # Fix invalid required attributes (not strictly necessary)
  # defp traverse_spec(
  #        %{"type" => schema_type, "required" => required} = value,
  #        ["schema" | _] = path
  #      )
  #      when schema_type != "object" and is_boolean(required) do
  #   value
  #   |> Map.delete("required")
  #   |> traverse_spec(path)
  # end

  # defp traverse_spec(
  #        %{"type" => schema_type, "required" => required} = value,
  #        [_name, "properties" | _] = path
  #      )
  #      when schema_type != "object" and is_boolean(required) do
  #   value
  #   |> Map.delete("required")
  #   |> traverse_spec(path)
  # end

  # defp traverse_spec(
  #        %{"type" => schema_type, "required" => required} = value,
  #        [_name, "schemas", "components"] = path
  #      )
  #      when schema_type != "object" and is_boolean(required) do
  #   value
  #   |> Map.delete("required")
  #   |> traverse_spec(path)
  # end

  # # Remove query parameters from path URLs
  # defp traverse_spec({url, value}, [url | ["paths"] = rest_path]) do
  #   url_new = url |> URI.parse() |> struct!(query: nil) |> URI.to_string()
  #   do_traverse_spec({url_new, value}, [url_new | rest_path])
  # end

  # defp traverse_spec(key_andor_value, path), do: do_traverse_spec(key_andor_value, path)

  # defp do_traverse_spec({key, map}, path) when is_map(map) do
  #   {key, traverse_spec(map, path)}
  # end

  # defp do_traverse_spec({key, list}, path) when is_list(list) do
  #   {key, traverse_spec(list, path)}
  # end

  # defp do_traverse_spec(map, path) when is_map(map) do
  #   Map.new(map, fn {key, value} -> traverse_spec({key, value}, [key | path]) end)
  # end

  # defp do_traverse_spec(list, path) when is_list(list) do
  #   list
  #   |> Enum.with_index()
  #   |> Enum.map(fn {value, index} -> traverse_spec(value, [[index] | path]) end)
  # end

  # defp do_traverse_spec(key_andor_value, _path), do: key_andor_value
end
