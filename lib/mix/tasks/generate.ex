defmodule Mix.Tasks.Generate do
  @moduledoc "Generates library's modules"
  use Mix.Task
  alias OpenAPIClient.Client.TypedDecoder
  alias Jason.OrderedObject

  @liqpay_base_url "https://www.liqpay.ua"

  @api_url "https://www.liqpay.ua/en/doc/api"

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
    inline_code_text_class: nil,
    table_classes: MapSet.new(),
    table_standalone_code_block_class: nil,
    standalone_code_block_class: nil,
    code_block_class: nil,
    irrelevant_code_block_class: nil,
    section_title_class: nil,
    section_subtitle_classes: MapSet.new()
  )

  Record.defrecordp(:menu_item,
    title: nil,
    id: nil,
    children: [],
    url: nil
  )

  @requirements ["app.start"]
  @shortdoc "Generates library's modules"
  def run(_) do
    {:ok, _} = Application.ensure_all_started(:wallaby)

    {:ok, session} = Wallaby.start_session()

    process_url(@api_url, session, [])
  end

  defp process_url(url, session, path) do
    IO.puts("Fetching `#{url}`")

    file =
      path
      |> List.flatten()
      |> List.insert_at(-1, "api")
      |> List.update_at(0, &"#{&1}.html")
      |> Enum.reverse()
      |> then(&["tmp" | &1])
      |> Path.join()

    {:ok, body} = http_request(url, file, session)
    {:ok, document} = parse_document(body)

    case parse_menu_page(body, document, session, path) do
      {:ok, menu_items} ->
        {:ok, menu_items}

      :error ->
        with [tab] <- Floki.find(document, "div.base-TabsList-root"),
             [_tab_label] =
               ~r/(?<!\w)new_doc_tab_lable__\w+(?!\w)/ |> Regex.run(body, capture: :first),
             [_ | _] = tab_items = Floki.find(tab, "button") do
          %URI{query: query} = uri = URI.new!(url)
          decoded_query = URI.decode_query(query)

          tab_index =
            decoded_query
            |> Map.fetch!("tab")
            |> parse_schema_value(%{type: :integer})

          tab_count = Enum.count(tab_items)

          if tab_count <= 2 or tab_index != 1 do
            process_page(body, document, path)
          else
            results =
              Enum.map(
                1..(tab_count - 1),
                fn
                  1 ->
                    {:ok, result} =
                      process_page(body, document, [[tab_index] | path])

                    result

                  other_index ->
                    query_new =
                      decoded_query
                      |> Map.put("tab", Integer.to_string(other_index))
                      |> URI.encode_query()

                    url_new = %URI{uri | query: query_new} |> URI.to_string()
                    {:ok, result} = process_url(url_new, session, [[other_index] | path])
                    result
                end
              )

            {:ok, results}
          end
        else
          _ -> :error
        end
    end
  end

  defp parse_menu_page(body, document, session, path) do
    with [menu_item_class] <-
           ~r/(?<!\w)new_doc_doc_menu_content__\w+(?!\w)/ |> Regex.run(body, capture: :first),
         menu_item_title_class when is_binary(menu_item_title_class) <-
           ~r/(?<!\w)new_doc_doc_list_title__\w+(?!\w)/
           |> Regex.run(body, capture: :first)
           |> (case do
                 nil ->
                   ~r/(?<!\w)new_doc_doc_title_link__\w+(?!\w)/
                   |> Regex.run(body, capture: :first)
                   |> case do
                     nil -> nil
                     [class] -> class
                   end

                 [class] ->
                   class
               end) do
      menu_items =
        document
        |> Floki.find("a.#{menu_item_class}")
        |> Enum.map(fn link ->
          [url] = Floki.attribute(link, "href")

          title =
            link
            |> Floki.find("div.#{menu_item_title_class} > div:first-child")
            |> parse_node_text(parse_options())
            |> String.replace(~r/\s+/, " ")
            |> String.trim()

          uri =
            @liqpay_base_url
            |> URI.merge(url)
            |> URI.new!()

          id = uri.path |> Path.split() |> List.last()

          query_new =
            (uri.query || "")
            |> URI.decode_query()
            |> Map.put("tab", "1")
            |> URI.encode_query()

          menu_item(title: title, id: id, url: URI.to_string(%URI{uri | query: query_new}))
        end)
        |> Enum.map(fn
          menu_item(id: id, url: url) = menu_item
          when id == "internet_acquiring" or
                 (hd(path) == "internet_acquiring") ->
            url
            |> process_url(session, [id | path])
            |> case do
              {:ok, children} -> menu_item(menu_item, children: children)
              :error -> menu_item
            end

          menu_item ->
            menu_item
        end)

      {:ok, menu_items}
    else
      _ -> :error
    end
  end

  defp process_page(body, document, path) do
    with [main_block_class] =
           ~r/(?<!\w)new_doc_page_doc__\w+(?!\w)/ |> Regex.run(body, capture: :first),
         [section_title_class] =
           ~r/(?<!\w)new_doc_integration_titles__\w+(?!\w)/ |> Regex.run(body, capture: :first),
         section_subtitle_classes =
           ~r/(?<!\w)(?:new_doc_possibilities_(?:text|subtitle)|doc_page_index_indent)__\w+(?!\w)/
           |> Regex.scan(body)
           |> Enum.map(fn [class] -> class end)
           |> MapSet.new(),
         [inline_code_text_class] =
           ~r/(?<!\w)new_doc_integration_code_text__\w+(?!\w)/ |> Regex.run(body, capture: :first),
         table_classes =
           ~r/(?<!\w)new_doc_table_scroll(?:_\w+)?__\w+(?!\w)/
           |> Regex.scan(body)
           |> Enum.map(fn [class] -> class end)
           |> Enum.uniq()
           |> MapSet.new(),
         ~r/(?<!\w)\w+(?!\w)/ |> Regex.run(body, capture: :first),
         table_standalone_code_block_class =
           ~r/(?<!\w)new_doc_table_code__\w+(?!\w)/
           |> Regex.run(body, capture: :first)
           |> (case do
                 [class] -> class
                 nil -> nil
               end),
         standalone_code_block_class =
           ~r/(?<!\w)new_doc_page_content__\w+(?!\w)/
           |> Regex.run(body, capture: :first)
           |> (case do
                 [class] -> class
                 nil -> nil
               end),
         code_block_class =
           ~r/(?<!\w)doc_code_style__\w+(?!\w)/
           |> Regex.run(body, capture: :first)
           |> (case do
                 [class] -> class
                 nil -> nil
               end),
         irrelevant_code_block_class =
           ~r/(?<!\w)new_doc_green_page_background__\w+(?!\w)/
           |> Regex.run(body, capture: :first)
           |> (case do
                 [class] -> class
                 nil -> nil
               end),
         json_file =
           path
           |> List.flatten()
           |> List.update_at(0, &"#{&1}.json")
           |> Enum.reverse()
           |> then(&["specs" | &1])
           |> Path.join(),
         :ok <-
           document
           |> Floki.find(
             "div.#{main_block_class} > div.MuiBox-root > div.MuiBox-root > div.MuiBox-root"
           )
           |> find_spec(
             parse_options(
               inline_code_text_class: inline_code_text_class,
               table_classes: table_classes,
               table_standalone_code_block_class: table_standalone_code_block_class,
               standalone_code_block_class: standalone_code_block_class,
               code_block_class: code_block_class,
               irrelevant_code_block_class: irrelevant_code_block_class,
               section_title_class: section_title_class,
               section_subtitle_classes: section_subtitle_classes
             ),
             path,
             json_file
           ) do
      {:ok, :ok}
    end
  end

  defp http_request(url, file, session) do
    file
    |> Path.dirname()
    |> File.mkdir_p!()

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
          |> then(&Regex.scan(~r/^\s*(['"])(.*)\1\s*$/s, &1, capture: :all_but_first))
          |> case do
            [[_quote, string]] ->
              string_new = String.replace(string, "\n", "\\n")
              ~s<"#{string_new}">

            [] ->
              str
          end

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
    |> String.replace(~r/"([\s\n]+)"/s, "\",\\1\"")
    |> String.replace(~r/^\s*(\{[^\{\[]+\[\{[^\]\}]+)\]\}([^\}\]]+\})\s*$/s, "\\1}]\\2")
  end

  defp parse_section(section, parse_options, was_request) do
    section
    |> parse_section_title(parse_options)
    |> process_section_title(
      section(node: section, is_request: was_request),
      parse_options,
      false
    )
  end

  defp parse_section_title(
         {_, _, _} = section,
         parse_options(
           section_title_class: section_title_class,
           section_subtitle_classes: section_subtitle_classes
         ) = parse_options
       ) do
    title = parse_section_title(section, parse_options, [section_title_class])
    subtitle = parse_section_title(section, parse_options, section_subtitle_classes)
    parse_section_title({title, subtitle}, parse_options)
  end

  defp parse_section_title({title, ""}, parse_options) when is_binary(title),
    do: parse_section_title({title, nil}, parse_options)

  defp parse_section_title({"", subtitle}, parse_options) when is_binary(subtitle),
    do: parse_section_title({nil, subtitle}, parse_options)

  defp parse_section_title({title, nil}, parse_options) when is_binary(title),
    do: parse_section_title(title, parse_options)

  defp parse_section_title({nil, subtitle}, parse_options) when is_binary(subtitle),
    do: parse_section_title(subtitle, parse_options)

  defp parse_section_title({title, subtitle}, _parse_options)
       when is_binary(title) and is_binary(subtitle) do
    {title, subtitle}
    |> downcase_section_title()
    |> case do
      {"options for generating data", _subtitle_downcase} -> subtitle
      {_title_downcase, "options for generating data"} -> title
      _ -> {title, subtitle}
    end
  end

  defp parse_section_title(title, _parse_options) when is_binary(title), do: title

  defp parse_section_title(section, parse_options, classes) do
    Enum.find_value(
      classes,
      fn class ->
        section
        |> Floki.find("div.#{class}")
        |> case do
          [div | _] -> parse_section_title_text(div, parse_options)
          [] -> nil
        end
      end
    )
  end

  defp parse_section_title_text(div, parse_options) do
    div
    |> parse_node_text(parse_options)
    |> String.trim_trailing(":")
  end

  defp downcase_section_title({title, subtitle}) when is_binary(title) and is_binary(subtitle),
    do: {String.downcase(title), String.downcase(subtitle)}

  defp downcase_section_title(title) when is_binary(title), do: String.downcase(title)

  defp process_section_title(title, section, parse_options, false) when is_binary(title) do
    ~r/^\s*(.*)\s+\(\s*(?:the\s+)?(object|array)\s+(\w+)\s*\)\s*$/i
    |> Regex.scan(title, capture: :all_but_first)
    |> case do
      [] ->
        ~r/^\s*(.*)\s+\(\s*(?:the\s+)?(\w+)\s+(object|array)\s*\)\s*$/i
        |> Regex.scan(title, capture: :all_but_first)
        |> case do
          [[description, name, type]] ->
            [[description, type, name]]

          [] ->
            ~r/^\s*Parameters\s+(\w+)\s*$/
            |> Regex.scan(title, capture: :all_but_first)
            |> case do
              [[name]] -> [["", "", name]]
              [] -> []
            end
        end

      [[description, type, name]] ->
        [[description, type, name]]
    end
    |> case do
      [[description, type, name]] ->
        {:ok,
         section(section,
           update_operation: :patch,
           update_type:
             if(type == "",
               do: :object,
               else: parse_property_type({"div", [], [type]}, parse_options)
             ),
           update_name: parse_property_name({"div", [], [name]}, parse_options),
           description: description
         )}

      [] ->
        title
        |> downcase_section_title()
        |> process_section_title(
          section(section, description: title),
          parse_options,
          true
        )
    end
  end

  # defp process_section_title({title, subtitle}, section, parse_options, false)
  #      when is_binary(title) and is_binary(subtitle) do
  #   {title, subtitle}
  #   |> downcase_section_title()
  #   |> process_section_title(
  #     section(section, description: subtitle),
  #     parse_options,
  #     true
  #   )
  # end

  defp process_section_title(nil, section, _parse_options, true),
    do: {:ok, section(section, update_operation: :patch)}

  defp process_section_title("main", section, parse_options, true),
    do: process_section_title(nil, section, parse_options, true)

  defp process_section_title("other parameters", section, parse_options, true),
    do: process_section_title(nil, section, parse_options, true)

  defp process_section_title(
         "parameters of splitting the payments",
         section,
         parse_options,
         true
       ),
       do: process_section_title(nil, section, parse_options, true)

  defp process_section_title(
         "parameters for tokenization within the token connect control",
         section,
         parse_options,
         true
       ),
       do: process_section_title(nil, section, parse_options, true)

  defp process_section_title("parameters for transfer to the card", section, parse_options, true),
    do: process_section_title(nil, section, parse_options, true)

  defp process_section_title(
         "parameters for transfer to the card's token",
         section,
         parse_options,
         true
       ),
       do: process_section_title(nil, section, parse_options, true)

  defp process_section_title("receiver parameters", section, parse_options, true),
    do: process_section_title(nil, section, parse_options, true)

  defp process_section_title("parameters for data formation", section, parse_options, true),
    do: process_section_title(nil, section, parse_options, true)

  defp process_section_title("sender parameters", section, _parse_options, true),
    do: {:ok, section(section, update_name: "sender")}

  defp process_section_title("regular payment parameters", section, _parse_options, true),
    do: {:ok, section(section, update_name: "regular_payment")}

  defp process_section_title("parameters for 1-click payment", section, _parse_options, true),
    do: {:ok, section(section, update_name: "one_click_payment")}

  defp process_section_title(
         "parameters for tokenization within the visa cards enrollment hub (vceh)",
         section,
         _parse_options,
         true
       ),
       do: {:ok, section(section, update_name: "vceh_tokenization")}

  defp process_section_title(
         "parameters for tokenization by card number",
         section,
         _parse_options,
         true
       ),
       do: {:ok, section(section, update_name: "card_tokenization")}

  defp process_section_title("response parameters", section, _parse_options, true),
    do: {:ok, section(section, is_request: false, update_operation: :patch)}

  defp process_section_title(
         "parameters for transfer to the current account",
         section,
         _parse_options,
         true
       ),
       do: {:ok, section(section, update_name: "receiver_account")}

  defp process_section_title(
         "parameters for aggregators",
         section,
         _parse_options,
         true
       ),
       do: {:ok, section(section, update_name: "aggregator")}

  defp process_section_title("api invoice_units", _section, _parse_options, true), do: :error

  defp process_section_title("payment widget parameters", _section, _parse_options, true),
    do: :error

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
         %{type: :object} = schema,
         section(update_operation: :patch, update_type: :object, update_name: "rro_info") =
           section_data,
         parse_options,
         false,
         [{:schema, :request}, "card_payment", "internet_acquiring"] = path
       ) do
    schema_new =
      schema
      |> append_schema_object_properties(
        OrderedObject.new([
          {"rro_info",
           %{
             type: :object,
             description: "Data for fiscalization"
           }}
        ])
      )
      |> process_section_properties(section_data, parse_options, path)

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
           table_classes: table_classes,
           table_standalone_code_block_class: table_standalone_code_block_class
         ) = parse_options,
         path
       ) do
    {properties, required} =
      table_classes
      |> Enum.find_value(fn class ->
        case Floki.find(node, "div.#{class}.MuiBox-root") do
          [table] -> table
          [] -> nil
        end
      end)
      |> Floki.find("table.MuiTable-root tbody.MuiTableBody-root tr.MuiTableRow-root")
      |> Enum.map(fn property ->
        [name, required, type, description | rest] =
          property
          |> Floki.find("td.MuiTableCell-root.MuiTableCell-body")
          |> case do
            [name, type, description | rest] when not is_request ->
              [name, {"div", [], ["Optional"]}, type, description | rest]

            full_property ->
              full_property
          end

        name = parse_property_name(name, parse_options)
        description = parse_node_text(description, parse_options)
        type = parse_property_type(type, parse_options)
        required = parse_property_required(required, parse_options)

        path_new = [name | path]

        property_schema =
          %{type: type, description: description}
          |> initialize_property_processing(path_new)
          |> parse_property_maximum_length()
          |> parse_property_enum()
          |> parse_property_examples()
          |> parse_property_separate_example(rest, path_new)

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
        schema_new = append_schema_object_properties(schema, properties_new)

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
          |> append_schema_object_properties(properties_new)

        items_new =
          if Enum.empty?(required_new) do
            items_new
          else
            Map.update(items_new, :required, required_new, &(&1 ++ required_new))
          end

        Map.put(schema, :items, items_new)
    end
  end

  defp parse_property_name(str, parse_options) do
    str |> parse_node_text(parse_options) |> String.trim()
  end

  defp parse_property_type(str, parse_options) do
    str
    |> parse_node_text(parse_options)
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

  defp parse_property_required(str, parse_options) do
    str
    |> parse_node_text(parse_options)
    |> String.trim()
    |> String.downcase()
    |> then(&Regex.scan(~r/^(required|optional)(\*)*$/, &1, capture: :all_but_first))
    |> case do
      [["required"]] -> true
      [["optional"]] -> false
      [["required", "*"]] -> false
      [["optional", "*"]] -> false
    end
  end

  defp initialize_property_processing(
         %{type: :string} = property,
         ["split_rules", {:schema, :request} | _] = path
       ) do
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

  # defp initialize_property_processing(
  #        %{type: :string} = property,
  #        [boolean_property, {:schema, _schema_type} | _] = path
  #      )
  #      when boolean_property in ~w(verifycode) and
  #             not is_map_key(property, :format) do
  #   property
  #   |> Map.put(:format, "boolean-yesno")
  #   |> initialize_property_processing(path)
  # end

  defp initialize_property_processing(
         %{type: :string} = property,
         [boolean_property, {:schema, _schema_type} | _] = path
       )
       when boolean_property in ~w(verifycode) and
              not is_map_key(property, :enum) do
    property
    |> Map.put(:enum, ["Y"])
    |> initialize_property_processing(path)
  end

  # defp initialize_property_processing(
  #        %{type: :string} = property,
  #        [boolean_property, {:schema, _schema_type} | _] = path
  #      )
  #      when boolean_property in ~w(subscribe prepare sandbox) and
  #             not is_map_key(property, :format) do
  #   property
  #   |> Map.put(:format, "boolean-integer")
  #   |> initialize_property_processing(path)
  # end

  # defp initialize_property_processing(
  #        %{type: :string} = property,
  #        ["recurringbytoken", "one_click_payment", {:schema, :request} | _] = path
  #      )
  #      when not is_map_key(property, :format) do
  #   property
  #   |> Map.put(:format, "boolean-integer")
  #   |> initialize_property_processing(path)
  # end

  defp initialize_property_processing(
         %{type: :string} = property,
         [boolean_property, {:schema, _schema_type} | _] = path
       )
       when boolean_property in ~w(subscribe prepare sandbox) and not is_map_key(property, :enum) do
    property
    |> Map.put(:enum, ["1"])
    |> initialize_property_processing(path)
  end

  defp initialize_property_processing(
         %{type: :string} = property,
         ["recurringbytoken", "one_click_payment", {:schema, :request} | _] = path
       )
       when not is_map_key(property, :enum) do
    property
    |> Map.put(:enum, ["1"])
    |> initialize_property_processing(path)
  end

  defp initialize_property_processing(
         %{type: :number} = property,
         [integer_property, {:schema, _schema_type} | _] = path
       )
       when integer_property in ~w(version mpi_eci) do
    property
    |> Map.put(:type, :integer)
    |> initialize_property_processing(path)
  end

  defp initialize_property_processing(
         %{type: :number} = property,
         ["id", [], "items", "rro_info", {:schema, :request} | _] = path
       ) do
    property
    |> Map.put(:type, :integer)
    |> initialize_property_processing(path)
  end

  defp initialize_property_processing(
         %{type: :string} = property,
         [url_property, {:schema, _schema_type} | _] = path
       )
       when url_property in ~w(result_url server_url product_url) and
              not is_map_key(property, :format) do
    property
    |> Map.put(:format, :uri)
    |> initialize_property_processing(path)
  end

  defp initialize_property_processing(
         %{type: :string} = property,
         [datetime_property, {:schema, _schema_type} | _] = path
       )
       when datetime_property in ~w(expired_date) and
              not is_map_key(property, :format) do
    property
    # |> Map.put(:format, "date-time-liqpay")
    |> Map.put(:format, "date-time")
    |> initialize_property_processing(path)
  end

  defp initialize_property_processing(
         %{type: :string} = property,
         ["subscribe_date_start", "regular_payment", {:schema, :request} | _] = path
       )
       when not is_map_key(property, :format) do
    property
    # |> Map.put(:format, "date-time-liqpay")
    |> Map.put(:format, "date-time")
    |> initialize_property_processing(path)
  end

  defp initialize_property_processing(
         %{type: :array} = property,
         ["delivery_emails", "rro_info", {:schema, :request} | _] = path
       )
       when not is_map_key(property, :items) do
    property
    |> Map.put(:items, %{type: :string, format: :email})
    |> initialize_property_processing(path)
  end

  defp initialize_property_processing(property, _path), do: property

  defp find_spec(
         blocks,
         parse_options(
           standalone_code_block_class: standalone_code_block_class,
           table_classes: table_classes
         ) = parse_options,
         path,
         json_file
       ) do
    {request_schema, response_schema, code_blocks} =
      blocks
      |> Enum.reduce({nil, nil, []}, fn section, {request_schema, response_schema, code_blocks} ->
        classes = node_classes(section)
        was_request = is_nil(response_schema)

        cond do
          standalone_code_block_class && Enum.member?(classes, standalone_code_block_class) ->
            section
            |> parse_section_title(parse_options)
            |> downcase_section_title()
            |> parse_standalone_example(section, path)
            |> case do
              {:ok, {is_request, code}} ->
                code_blocks_new = [{is_request, code} | code_blocks]
                {request_schema, response_schema, code_blocks_new}

              :error ->
                {request_schema, response_schema, code_blocks}
            end

          Enum.find_value(table_classes, fn class ->
            case Floki.find(section, "div.#{class}.MuiBox-root") do
              [table] -> table
              [] -> nil
            end
          end) ->
            section
            |> parse_section(parse_options, was_request)
            |> case do
              {:ok,
               section(is_request: is_request, update_operation: update_operation) = section_data} ->
                {section_schema_type, section_schema} =
                  if is_request do
                    {:request, request_schema}
                  else
                    {:response, response_schema}
                  end

                section_schema_new =
                  (section_schema || %{type: :object, properties: OrderedObject.new([])})
                  |> update_section_schema(section_data, parse_options, false, [
                    {:schema, section_schema_type} | path
                  ])
                  |> case do
                    {true, section_schema_new} ->
                      section_schema_new

                    {false, section_schema_new} when update_operation != :patch ->
                      section_schema_new
                  end

                if is_request,
                  do: {section_schema_new, response_schema, code_blocks},
                  else: {request_schema, section_schema_new, code_blocks}

              :error ->
                {request_schema, response_schema, code_blocks}
            end

          :else ->
            {_, _, code_blocks_new} =
              search_code_blocks(section, parse_options, {nil, nil, code_blocks}, path)

            {request_schema, response_schema, code_blocks_new}
        end
      end)

    {request_schema_new, response_schema_new} =
      code_blocks
      |> Enum.reverse()
      |> Enum.reduce({request_schema, response_schema}, fn
        {is_request, example}, {request_schema, response_schema} ->
          schema = if is_request, do: request_schema, else: response_schema
          schema_new = patch_schema_examples(example, schema)
          if is_request, do: {schema_new, response_schema}, else: {request_schema, schema_new}
      end)

    response_schema_new = response_schema_new || %{type: :object}

    json_file
    |> Path.dirname()
    |> File.mkdir_p!()

    OrderedObject.new(
      openapi: "3.1.0",
      info: OrderedObject.new(version: "3", title: "External API"),
      servers: [OrderedObject.new(url: "https://liqpay.ua")],
      paths:
        OrderedObject.new([
          {"/test",
           OrderedObject.new(
             post:
               OrderedObject.new(
                 summary: "Checkout",
                 operationId: "checkout",
                 requestBody:
                   OrderedObject.new(
                     content:
                       OrderedObject.new([
                         {"application/json", OrderedObject.new(schema: request_schema_new)}
                       ])
                   ),
                 responses:
                   OrderedObject.new([
                     {"200",
                      OrderedObject.new(
                        description: "200",
                        content:
                          OrderedObject.new([
                            {"application/json", OrderedObject.new(schema: response_schema_new)}
                          ])
                      )}
                   ])
               )
           )}
        ])
    )
    |> Jason.encode!(pretty: true)
    |> then(&File.write!(json_file, &1))
  end

  defp search_code_blocks(
         {"div", _, _} = div,
         parse_options(
           section_title_class: section_title_class,
           section_subtitle_classes: section_subtitle_classes,
           code_block_class: code_block_class,
           irrelevant_code_block_class: irrelevant_code_block_class
         ) = parse_options,
         {title, subtitle, code_blocks},
         path
       ) do
    classes =
      div
      |> node_classes()
      |> MapSet.new()

    cond do
      MapSet.member?(classes, section_title_class) ->
        title_new = parse_section_title_text(div, parse_options)
        {title_new, subtitle, code_blocks}

      not (classes
           |> MapSet.intersection(section_subtitle_classes)
           |> Enum.empty?()) ->
        subtitle_new = parse_section_title_text(div, parse_options)
        {title, subtitle_new, code_blocks}

      MapSet.member?(classes, code_block_class) ->
        {title, subtitle}
        |> parse_section_title(parse_options)
        |> downcase_section_title()
        |> parse_standalone_example(div, path)
        |> case do
          {:ok, {is_request, code}} ->
            code_blocks_new = [{is_request, code} | code_blocks]
            {title, nil, code_blocks_new}

          :error ->
            {title, subtitle, code_blocks}
        end

      irrelevant_code_block_class && MapSet.member?(classes, irrelevant_code_block_class) ->
        {title, subtitle, code_blocks}

      :else ->
        div
        |> Floki.children()
        |> Enum.reduce(
          {title, subtitle, code_blocks},
          &search_code_blocks(&1, parse_options, &2, path)
        )
    end
  end

  defp search_code_blocks(_node, _parse_options, acc, _path), do: acc

  defp parse_property_separate_example(schema, [], _path) do
    schema
  end

  defp parse_property_separate_example(
         %{description: description} = schema,
         [
           {"code", _, _} = code
         ],
         path
       ) do
    case path do
      ["dae", {:schema, :request} | _] ->
        description_new =
          "#{description |> String.split("\n") |> hd()}\n\nPossible `JSON` object:\n```\n#{code |> Floki.text() |> String.trim()}\n```"

        %{schema | description: description_new}

      ["split_rules", {:schema, :request} | _] ->
        description_new =
          String.replace(description, ~r/\.?\s*Example\s+(`)?JSON(?(1)\1|)\s+string:?\s*$/, "")

        code
        |> extract_code()
        |> Jason.decode!()
        |> patch_schema_examples(%{schema | description: description_new})

      ["goods", {:schema, :request}, [1], "invoice", "internet_acquiring"] ->
        code
        |> extract_code()
        |> Jason.decode!()
        |> patch_schema_examples(schema)
    end
  end

  defp parse_property_separate_example(schema, [{_, _, _} = node], path) do
    parse_property_separate_example(schema, Floki.find(node, "code.language-json"), path)
  end

  defp patch_schema_examples(example, %{type: :object, properties: properties} = schema)
       when is_map(example) do
    properties_new =
      Enum.reduce(
        example,
        properties,
        fn {key, value}, properties ->
          case get_in(properties, [key]) do
            nil ->
              IO.inspect(value, label: "Unused example (#{key})")
              properties

            current ->
              put_in(properties, [key], patch_schema_examples(value, current))
          end
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

  defp parse_property_maximum_length(%{description: description} = property) do
    ~r/(?:\.\s+)?(?:The\s+m|M)ax(?:imum)?\s+length(?:\s+is)?\s+(\*\*)?(\d+)\1?\s+symbols/
    |> Regex.scan(description)
    |> case do
      [[full_match, "", max_length]] ->
        description_new = String.replace(description, full_match, "")

        Map.merge(property, %{
          maxLength: String.to_integer(max_length),
          description: description_new
        })

      [[full_match, "**", max_length]] ->
        description_new = String.replace(description, full_match, "")

        Map.merge(property, %{
          maxLength: String.to_integer(max_length),
          description: description_new
        })

      [] ->
        property
    end
  end

  defp parse_property_enum(%{description: description} = property) do
    ~r/(\.\s+)?((?:Possible|Present|Valid)\s+values?\s*:?|Current\s+value\s*\-?|^Customer's\s+language)\n?([^\.\n]+)(?:\.|$)/i
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
            parse_schema_value(key, property)
          end)
          |> Enum.uniq()

        property_new =
          Map.merge(property, %{
            enum: enum,
            description: description_new
          })

        if not has_descriptions and length(enum_options) == 1 and
             prefix_text |> String.replace(~r/\s+/, " ") |> String.starts_with?("Current value") do
          [{default, _}] = enum_options
          default_new = parse_schema_value(default, property_new)
          Map.put(property_new, :default, default_new)
        else
          property_new
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

          property
          |> Map.update(:enum, enum, &Enum.uniq(&1 ++ enum))
          |> Map.delete(:description)
        else
          ~r/^\s*`([^`]+)`\s*-\s*(.+)$/m
          |> Regex.scan(description)
          |> Enum.reduce(property, fn [full_match, key, key_description],
                                      %{description: description} = property_new ->
            description_new =
              String.replace(description, full_match, "* `#{key}` - #{key_description}")

            key_new = parse_schema_value(key, property_new)

            property_new
            |> Map.update(:enum, [key_new], &Enum.uniq(&1 ++ [key_new]))
            |> Map.put(:description, description_new)
          end)
        end
    end
  end

  defp parse_property_examples(%{description: description} = property) do
    ~r/(?:\.\s+)?(?:For\s+example):?((?:\s*`[^`]+?`,?)+)\s*(?:\(([^\)]+)\))?(?:\.|$)/
    |> Regex.scan(description)
    |> case do
      [[full_match, examples_match]] ->
        process_examples_match_in_description(property, full_match, examples_match)

      [[full_match, examples_match, explanation]] ->
        description_new = String.replace(description, full_match, "#{full_match}. #{explanation}")
        property_new = %{property | description: description_new}
        process_examples_match_in_description(property_new, full_match, examples_match)

      [] ->
        property
    end
  end

  defp parse_property_examples(property), do: property

  defp process_examples_match_in_description(
         %{description: description} = property,
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
        |> parse_schema_value(property)
      end)
      |> Enum.uniq()

    description_new =
      description
      |> String.replace(full_match, "")
      |> String.trim()

    Map.merge(property, %{examples: examples, description: description_new})
  end

  defp node_classes(node) do
    node
    |> Floki.attribute("class")
    |> Enum.flat_map(&String.split/1)
    |> Enum.uniq()
  end

  defp parse_schema_value(value, %{type: type} = _schema) do
    {:ok, value_decoded} =
      TypedDecoder.decode(
        value,
        type,
        [],
        TypedDecoder
      )

    value_decoded
  end

  defp append_schema_object_properties(
         schema,
         %OrderedObject{values: values_new} = properties_new
       ) do
    Map.update(schema, :properties, properties_new, fn %OrderedObject{values: values_old} ->
      OrderedObject.new(values_old ++ values_new)
    end)
  end

  defp parse_standalone_example("response example", _div, [[2], "gpay", "internet_acquiring"]),
    do: :error

  defp parse_standalone_example("response example", div, path),
    do: parse_standalone_example(false, div, path)

  defp parse_standalone_example(title, div, path) when is_binary(title),
    do: parse_standalone_example(true, div, path)

  defp parse_standalone_example(is_request, div, _path) when is_boolean(is_request) do
    div
    |> Floki.find("code.language-json")
    |> case do
      [code_string] ->
        {:ok, {is_request, code_string |> extract_code() |> Jason.decode!()}}

      [] ->
        div
        |> Floki.find("code.language-javascript")
        |> case do
          [code] ->
            [[code_string]] =
              code
              |> extract_code()
              |> then(
                &Regex.scan(
                  ~r/liqpay\.(?:cnb_form\(|api\(\s*\"request\"\s*,)\s*(\{(?:[^}{]+|(?1))*+\})/,
                  &1,
                  capture: :all_but_first
                )
              )

            {:ok, {is_request, Jason.decode!(code_string)}}

          [] ->
            :error
        end
    end
  end

  defp parse_node_text([node], parse_options), do: parse_node_text(node, parse_options)

  defp parse_node_text(
         node,
         parse_options(inline_code_text_class: inline_code_text_class) = parse_options
       ) do
    node
    |> Floki.children()
    |> Enum.map_join(fn
      str when is_binary(str) ->
        result = String.replace(str, ~r/\s+/, " ")

        if result == " " and String.contains?(str, "\n") do
          "\n"
        else
          result
        end

      {"a", _attrs, _children} = link ->
        [href] = Floki.attribute(link, "href")

        "[#{link |> parse_node_text(parse_options) |> String.replace(~r/\s+/, " ") |> String.trim()}](#{@liqpay_base_url |> URI.merge(href) |> URI.to_string()})"

      {"br", _attrs, _children} ->
        "\n"

      {"b", _attrs, [text]} = _bold ->
        "**#{text}**"

      {"span", _attrs, [text]} = span ->
        span
        |> node_classes()
        |> Enum.member?(inline_code_text_class)
        |> if(do: "`#{text}`", else: text)

      {"div", _attrs, _children} = div ->
        div
        |> parse_node_text(parse_options)
        |> String.trim_trailing()
        |> Kernel.<>("\n")
    end)
    |> String.replace(~r/\n\s+([^[:upper:]`])/, " \\1")
    |> String.trim()
  end
end
