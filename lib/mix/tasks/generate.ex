defmodule Mix.Tasks.Generate do
  @moduledoc "Generates library's modules"
  use Mix.Task
  alias OpenAPIClient.Client.TypedDecoder
  alias Jason.OrderedObject

  @liqpay_base_url "https://www.liqpay.ua"

  @api_url "#{@liqpay_base_url}/en/doc/api"

  require Record

  Record.defrecordp(:block,
    node: nil,
    is_request: true,
    update_operation: :new,
    update_type: :object,
    update_name: nil,
    description: nil
  )

  Record.defrecordp(:block_parse_settings,
    inline_code_text_class: nil,
    table_classes: MapSet.new(),
    table_standalone_code_block_class: nil,
    standalone_code_block_class: nil,
    code_block_class: nil,
    irrelevant_code_block_class: nil,
    title_class: nil,
    subtitle_classes: MapSet.new()
  )

  Record.defrecordp(:section,
    type: nil,
    title: nil,
    id: nil,
    children: [],
    url: nil
  )

  Record.defrecordp(:endpoint,
    title: nil,
    id: nil,
    request: nil,
    response: nil
  )

  Record.defrecordp(:parse_settings,
    url: nil,
    body: nil,
    document: nil,
    session: nil,
    path: []
  )

  @requirements ["app.start"]
  @shortdoc "Generates library's modules"
  def run(_) do
    {:ok, _} = Application.ensure_all_started(:wallaby)

    {:ok, session} = Wallaby.start_session()

    process_url(@api_url, parse_settings(session: session))
  end

  defp process_url(url, parse_settings(session: session, path: path) = parse_settings) do
    IO.puts("Fetching `#{url}`")

    file =
      path
      |> List.insert_at(-1, "api")
      |> List.update_at(0, &"#{&1}.html")
      |> Enum.reverse()
      |> then(&["tmp" | &1])
      |> Path.join()

    {:ok, body} = fetch_url(url, file, session)
    {:ok, document} = parse_document(body)

    parse_page(parse_settings(parse_settings, url: url, body: body, document: document))
  end

  defp fetch_url(url, file, session) do
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

  defp parse_page(parse_settings) do
    parse_settings
    |> parse_menu_page()
    |> case do
      {:ok, _} = result -> result
      :error -> parse_doc_page(parse_settings)
    end
  end

  defp parse_menu_page(
         parse_settings(body: body, document: document, path: path) = parse_settings
       ) do
    with [item_class] <-
           Regex.run(~r/(?<!\w)new_doc_doc_menu_content__\w+(?!\w)/, body, capture: :first),
         [_ | _] = links <- Floki.find(document, "a.#{item_class}") do
      item_title_class =
        ~r/(?<!\w)new_doc_doc_list_title__\w+(?!\w)/
        |> Regex.run(body, capture: :first)
        |> case do
          nil ->
            [class] =
              Regex.run(~r/(?<!\w)new_doc_doc_title_link__\w+(?!\w)/, body, capture: :first)

            class

          [class] ->
            class
        end

      links
      |> Enum.map(fn link ->
        [url] = Floki.attribute(link, "href")

        title =
          link
          |> Floki.find("div.#{item_title_class} > div:first-child")
          |> parse_node_text(block_parse_settings())
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

        url_new = URI.to_string(%URI{uri | query: query_new})

        item = section(type: :menu, title: title, id: id, url: url_new)

        if id == "information" or
             (path != [] and hd(path) == "information") do
          url_new
          |> process_url(parse_settings(parse_settings, path: [id | path]))
          |> case do
            {:ok, children} -> section(item, children: children)
            :error -> item
          end
        else
          item
        end
      end)
      |> then(&{:ok, &1})
    else
      _ -> :error
    end
  end

  defp parse_doc_page(
         parse_settings(url: url, body: body, document: document, path: path) = parse_settings
       ) do
    document
    |> Floki.find("div.base-TabsList-root")
    |> case do
      [tab] ->
        [tab_label_class] =
          Regex.run(~r/(?<!\w)new_doc_tab_lable__\w+(?!\w)/, body, capture: :first)

        [_ | _] = tab_items = Floki.find(tab, "button")

        %URI{query: query} = uri = URI.new!(url)
        decoded_query = URI.decode_query(query)

        tab_index =
          decoded_query
          |> Map.fetch!("tab")
          |> parse_schema_value(%{type: :integer})

        tab_count = Enum.count(tab_items)

        cond do
          tab_count <= 2 ->
            "documentation" =
              tab_items
              |> Enum.at(tab_index)
              |> Floki.find("div.#{tab_label_class}")
              |> parse_node_text(block_parse_settings())
              |> downcase_block_title()

            process_doc(parse_settings)

          tab_index != 1 ->
            parse_settings
            |> parse_settings(path: path)
            |> process_doc()

          :else ->
            results =
              Enum.map(
                1..(tab_count - 1),
                fn tab_index ->
                  tab_label =
                    tab_items
                    |> Enum.at(tab_index)
                    |> Floki.find("div.#{tab_label_class}")
                    |> parse_node_text(block_parse_settings())

                  {:ok, block(update_operation: :new_endpoint, update_name: tab_id)} =
                    tab_label
                    |> downcase_block_title()
                    |> process_block_title(block(), block_parse_settings(), true, path)

                  parse_settings_new = parse_settings(parse_settings, path: [tab_id | path])

                  {:ok, result} =
                    if tab_index == 1 do
                      process_doc(parse_settings_new)
                    else
                      query_new =
                        decoded_query
                        |> Map.put("tab", Integer.to_string(tab_index))
                        |> URI.encode_query()

                      url_new = %URI{uri | query: query_new} |> URI.to_string()

                      process_url(url_new, parse_settings_new)
                    end

                  result
                end
              )

            {:ok, results}
        end

      [] ->
        :error
    end
  end

  defp process_doc(parse_settings(body: body, document: document, path: path) = _parse_settings) do
    [main_block_class] = Regex.run(~r/(?<!\w)new_doc_page_doc__\w+(?!\w)/, body, capture: :first)

    [title_class] =
      Regex.run(~r/(?<!\w)new_doc_integration_titles__\w+(?!\w)/, body, capture: :first)

    subtitle_classes =
      ~r/(?<!\w)(?:new_doc_possibilities_(?:text|subtitle)|doc_page_index_indent)__\w+(?!\w)/
      |> Regex.scan(body)
      |> Enum.map(fn [class] -> class end)
      |> MapSet.new()

    [inline_code_text_class] =
      Regex.run(~r/(?<!\w)new_doc_integration_code_text__\w+(?!\w)/, body, capture: :first)

    table_classes =
      ~r/(?<!\w)new_doc_table_scroll(?:_\w+)?__\w+(?!\w)/
      |> Regex.scan(body)
      |> Enum.map(fn [class] -> class end)
      |> Enum.uniq()
      |> MapSet.new()

    table_standalone_code_block_class =
      ~r/(?<!\w)new_doc_table_code__\w+(?!\w)/
      |> Regex.run(body, capture: :first)
      |> case do
        [class] -> class
        nil -> nil
      end

    standalone_code_block_class =
      ~r/(?<!\w)new_doc_page_content__\w+(?!\w)/
      |> Regex.run(body, capture: :first)
      |> case do
        [class] -> class
        nil -> nil
      end

    code_block_class =
      ~r/(?<!\w)doc_code_style__\w+(?!\w)/
      |> Regex.run(body, capture: :first)
      |> case do
        [class] -> class
        nil -> nil
      end

    irrelevant_code_block_class =
      ~r/(?<!\w)new_doc_green_page_background__\w+(?!\w)/
      |> Regex.run(body, capture: :first)
      |> case do
        [class] -> class
        nil -> nil
      end

    json_file =
      path
      |> List.update_at(0, &"#{&1}.json")
      |> Enum.reverse()
      |> then(&["specs" | &1])
      |> Path.join()

    document
    |> Floki.find("div.#{main_block_class} > div.MuiBox-root > div.MuiBox-root > div.MuiBox-root")
    |> parse_doc(
      block_parse_settings(
        inline_code_text_class: inline_code_text_class,
        table_classes: table_classes,
        table_standalone_code_block_class: table_standalone_code_block_class,
        standalone_code_block_class: standalone_code_block_class,
        code_block_class: code_block_class,
        irrelevant_code_block_class: irrelevant_code_block_class,
        title_class: title_class,
        subtitle_classes: subtitle_classes
      ),
      path,
      json_file
    )
    |> case do
      :ok -> {:ok, :ok}
      :error -> :error
    end
  end

  defp parse_doc(
         block_nodes,
         block_parse_settings(
           standalone_code_block_class: standalone_code_block_class,
           table_classes: table_classes
         ) = block_parse_settings,
         path,
         json_file
       ) do
    {endpoints, code_blocks, _path} =
      Enum.reduce(block_nodes, {[], [], path}, fn node, {endpoints, code_blocks, path} ->
        classes = node_classes(node)

        cond do
          standalone_code_block_class && Enum.member?(classes, standalone_code_block_class) ->
            node
            |> parse_block_title(block_parse_settings)
            |> downcase_block_title()
            |> parse_standalone_example(node, path)
            |> case do
              {:ok, {is_request, code}} ->
                code_blocks_new = [{is_request, code} | code_blocks]
                {endpoints, code_blocks_new, path}

              :error ->
                {endpoints, code_blocks, path}
            end

          Enum.find_value(table_classes, fn class ->
            case Floki.find(node, "div.#{class}.MuiBox-root") do
              [table] -> table
              [] -> nil
            end
          end) ->
            was_request =
              case endpoints do
                [] -> true
                [endpoint(response: response_schema) | _] -> is_nil(response_schema)
              end

            node
            |> parse_block_data(block_parse_settings, was_request, path)
            |> case do
              {:ok, block(update_operation: update_operation) = block_data} ->
                {is_request, [top_endpoint | rest_endpoints] = _endpoints_new, path_new} =
                  case block_data do
                    block(update_operation: :new_endpoint, update_name: id) ->
                      [top_id | rest_path] = path
                      id = if(endpoints == [], do: top_id, else: id)
                      endpoint = endpoint(id: id)
                      endpoints_new = [endpoint | endpoints]
                      path_new = [id | rest_path]
                      {true, endpoints_new, path_new}

                    block(is_request: is_request) ->
                      {is_request, endpoints, path}
                  end

                {block_schema_type, block_schema} =
                  if is_request do
                    endpoint(request: request_schema) = top_endpoint
                    {:request, request_schema}
                  else
                    endpoint(response: response_schema) = top_endpoint
                    {:response, response_schema}
                  end

                block_schema_new =
                  (block_schema || %{type: :object, properties: OrderedObject.new([])})
                  |> update_block_schema(block_data, block_parse_settings, false, [
                    {:schema, block_schema_type} | path_new
                  ])
                  |> case do
                    {true, block_schema_new} ->
                      block_schema_new

                    {false, block_schema_new} when update_operation != :patch ->
                      block_schema_new
                  end

                top_endpoint_new =
                  if is_request,
                    do: endpoint(top_endpoint, request: block_schema_new),
                    else: endpoint(top_endpoint, response: block_schema_new)

                endpoints_new = [top_endpoint_new | rest_endpoints]

                {endpoints_new, code_blocks, path_new}

              :error ->
                {endpoints, code_blocks, path}
            end

          :else ->
            {_, _, code_blocks_new} =
              search_code_blocks(node, block_parse_settings, {nil, nil, code_blocks}, path)

            {endpoints, code_blocks_new, path}
        end
      end)

    [top_endpoint | rest_endpoints] = _endpoints = Enum.reverse(endpoints)

    top_endpoint_new =
      code_blocks
      |> Enum.reverse()
      |> Enum.reduce(top_endpoint, fn
        {is_request, example},
        endpoint(request: request_schema, response: response_schema) = endpoint ->
          schema = if is_request, do: request_schema, else: response_schema
          schema_new = patch_schema_examples(example, schema)

          if is_request,
            do: endpoint(endpoint, request: schema_new),
            else: endpoint(endpoint, response: schema_new)
      end)

    json_file
    |> Path.dirname()
    |> File.mkdir_p!()

    OrderedObject.new(
      openapi: "3.1.0",
      info: OrderedObject.new(version: "3", title: "External API"),
      servers: [OrderedObject.new(url: "https://liqpay.ua")],
      paths:
        [top_endpoint_new | rest_endpoints]
        |> Enum.map(fn endpoint(request: request_schema, response: response_schema) ->
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
                         {"application/json", OrderedObject.new(schema: request_schema)}
                       ])
                   ),
                 responses:
                   OrderedObject.new([
                     {"200",
                      OrderedObject.new(
                        description: "200",
                        content:
                          OrderedObject.new([
                            {"application/json",
                             OrderedObject.new(schema: response_schema || %{type: :object})}
                          ])
                      )}
                   ])
               )
           )}
        end)
        |> OrderedObject.new()
    )
    |> Jason.encode!(pretty: true)
    |> then(&File.write!(json_file, &1))
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

  defp parse_block_data(node, block_parse_settings, was_request, path) do
    node
    |> parse_block_title(block_parse_settings)
    |> process_block_title(
      block(node: node, is_request: was_request),
      block_parse_settings,
      false,
      path
    )
  end

  defp parse_block_title(
         {_, _, _} = node,
         block_parse_settings(
           title_class: title_class,
           subtitle_classes: subtitle_classes
         ) = block_parse_settings
       ) do
    title = parse_block_title(node, block_parse_settings, [title_class])
    subtitle = parse_block_title(node, block_parse_settings, subtitle_classes)
    parse_block_title({title, subtitle}, block_parse_settings)
  end

  defp parse_block_title({title, ""}, block_parse_settings) when is_binary(title),
    do: parse_block_title({title, nil}, block_parse_settings)

  defp parse_block_title({"", subtitle}, block_parse_settings) when is_binary(subtitle),
    do: parse_block_title({nil, subtitle}, block_parse_settings)

  defp parse_block_title({title, nil}, block_parse_settings) when is_binary(title),
    do: parse_block_title(title, block_parse_settings)

  defp parse_block_title({nil, subtitle}, block_parse_settings) when is_binary(subtitle),
    do: parse_block_title(subtitle, block_parse_settings)

  defp parse_block_title({title, subtitle}, _block_parse_settings)
       when is_binary(title) and is_binary(subtitle) do
    {title, subtitle}
    |> downcase_block_title()
    |> case do
      {"options for generating data", _subtitle_downcase} -> subtitle
      {_title_downcase, "options for generating data"} -> title
      _ -> {title, subtitle}
    end
  end

  defp parse_block_title(title, _block_parse_settings) when is_binary(title), do: title

  defp parse_block_title(node, block_parse_settings, classes) do
    Enum.find_value(
      classes,
      fn class ->
        node
        |> Floki.find("div.#{class}")
        |> case do
          [div | _] -> parse_block_title_text(div, block_parse_settings)
          [] -> nil
        end
      end
    )
  end

  defp parse_block_title_text(div, block_parse_settings) do
    div
    |> parse_node_text(block_parse_settings)
    |> String.trim_trailing(":")
  end

  defp downcase_block_title({title, subtitle}) when is_binary(title) and is_binary(subtitle),
    do: {String.downcase(title), String.downcase(subtitle)}

  defp downcase_block_title(title) when is_binary(title), do: String.downcase(title)

  defp process_block_title(title, block, block_parse_settings, false, path)
       when is_binary(title) do
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
         block(block,
           update_operation: :patch,
           update_type:
             if(type == "",
               do: :object,
               else: parse_property_type({"div", [], [type]}, block_parse_settings)
             ),
           update_name: parse_property_name({"div", [], [name]}, block_parse_settings),
           description: description
         )}

      [] ->
        title
        |> downcase_block_title()
        |> process_block_title(
          block(block, description: title),
          block_parse_settings,
          true,
          path
        )
    end
  end

  # defp process_block_title({title, subtitle}, block, block_parse_settings, false, path)
  #      when is_binary(title) and is_binary(subtitle) do
  #   {title, subtitle}
  #   |> downcase_block_title()
  #   |> process_block_title(
  #     block(block, description: subtitle),
  #     block_parse_settings,
  #     true, path
  #   )
  # end

  defp process_block_title(
         "encrypted token",
         block(node: nil) = block,
         _block_parse_settings,
         true,
         ["apay", "internet_acquiring"] = _path
       ),
       do: {:ok, block(block, update_operation: :new_endpoint, update_name: "encrypted_token")}

  defp process_block_title(
         "decrypted token",
         block(node: nil) = block,
         _block_parse_settings,
         true,
         ["apay", "internet_acquiring"] = _path
       ),
       do: {:ok, block(block, update_operation: :new_endpoint, update_name: "decrypted_token")}

  defp process_block_title(
         "encrypted token",
         block(node: nil) = block,
         _block_parse_settings,
         true,
         ["gpay", "internet_acquiring"] = _path
       ),
       do: {:ok, block(block, update_operation: :new_endpoint, update_name: "encrypted_token")}

  defp process_block_title(
         "decrypted token",
         block(node: nil) = block,
         _block_parse_settings,
         true,
         ["gpay", "internet_acquiring"] = _path
       ),
       do: {:ok, block(block, update_operation: :new_endpoint, update_name: "decrypted_token")}

  defp process_block_title(
         "create subscribtion",
         block(node: nil) = block,
         _block_parse_settings,
         true,
         ["subscription", "internet_acquiring"] = _path
       ),
       do: {:ok, block(block, update_operation: :new_endpoint, update_name: "create")}

  defp process_block_title(
         "unsubscribe",
         block(node: nil) = block,
         _block_parse_settings,
         true,
         [
           "subscription",
           "internet_acquiring"
         ] = _path
       ),
       do: {:ok, block(block, update_operation: :new_endpoint, update_name: "unsubscribe")}

  defp process_block_title(
         "edit subscribtion",
         block(node: nil) = block,
         _block_parse_settings,
         true,
         ["subscription", "internet_acquiring"] = _path
       ),
       do: {:ok, block(block, update_operation: :new_endpoint, update_name: "edit")}

  defp process_block_title(
         "funds blocking",
         block(node: nil) = block,
         _block_parse_settings,
         true,
         ["two_step", "internet_acquiring"] = _path
       ),
       do: {:ok, block(block, update_operation: :new_endpoint, update_name: "block")}

  defp process_block_title(
         "completion of payment",
         block(node: nil) = block,
         _block_parse_settings,
         true,
         ["two_step", "internet_acquiring"] = _path
       ),
       do: {:ok, block(block, update_operation: :new_endpoint, update_name: "complete")}

  defp process_block_title(
         "issuing the invoice",
         block(node: nil) = block,
         _block_parse_settings,
         true,
         ["invoice", "internet_acquiring"] = _path
       ),
       do: {:ok, block(block, update_operation: :new_endpoint, update_name: "issue")}

  defp process_block_title(
         "invoice cancelation",
         block(node: nil) = block,
         _block_parse_settings,
         true,
         ["invoice", "internet_acquiring"] = _path
       ),
       do: {:ok, block(block, update_operation: :new_endpoint, update_name: "cancel")}

  defp process_block_title(
         "company creation create",
         block(node: nil) = block,
         _block_parse_settings,
         true,
         ["shop_create", "partnership"] = _path
       ),
       do: {:ok, block(block, update_operation: :new_endpoint, update_name: "create")}

  defp process_block_title(
         "company creation register",
         block(node: nil) = block,
         _block_parse_settings,
         true,
         ["shop_create", "partnership"] = _path
       ),
       do: {:ok, block(block, update_operation: :new_endpoint, update_name: "register")}

  defp process_block_title("main", block, _block_parse_settings, true, _path),
    do: {:ok, block(block, update_operation: :new_endpoint)}

  defp process_block_title(
         "other parameters" = title,
         block,
         block_parse_settings,
         true,
         [_, section, "internet_acquiring"] = _path
       ),
       do:
         process_block_title(title, block, block_parse_settings, true, [
           section,
           "internet_acquiring"
         ])

  defp process_block_title(
         "other parameters",
         block,
         _block_parse_settings,
         true,
         [_, "internet_acquiring"] = _path
       ),
       do: {:ok, block(block, update_operation: :patch)}

  defp process_block_title(
         "other parameters" = title,
         block,
         block_parse_settings,
         true,
         ["transferring_to_card"] = _path
       ),
       do:
         process_block_title(title, block, block_parse_settings, true, [
           "checkout",
           "internet_acquiring"
         ])

  defp process_block_title(
         "parameters of splitting the payments" = title,
         block,
         block_parse_settings,
         true,
         [_, section, "internet_acquiring"] = _path
       ),
       do:
         process_block_title(title, block, block_parse_settings, true, [
           section,
           "internet_acquiring"
         ])

  defp process_block_title(
         "parameters of splitting the payments",
         block,
         _block_parse_settings,
         true,
         [_, "internet_acquiring"] = _path
       ),
       do: {:ok, block(block, update_operation: :patch)}

  defp process_block_title(
         "parameters for tokenization within the token connect control",
         block,
         _block_parse_settings,
         true,
         ["obtain", "tokens"] = _path
       ),
       do: {:ok, block(block, update_operation: :patch)}

  defp process_block_title(
         "parameters for transfer to the card",
         block,
         _block_parse_settings,
         true,
         ["transferring_to_card"] = _path
       ),
       do: {:ok, block(block, update_operation: :patch)}

  defp process_block_title(
         "parameters for transfer to the card's token",
         block,
         _block_parse_settings,
         true,
         ["transferring_to_card"] = _path
       ),
       do: {:ok, block(block, update_operation: :patch)}

  defp process_block_title(
         "receiver parameters",
         block,
         _block_parse_settings,
         true,
         ["transferring_to_card"] = _path
       ),
       do: {:ok, block(block, update_operation: :patch)}

  defp process_block_title(
         "parameters for data formation",
         block,
         _block_parse_settings,
         true,
         ["decrypted_token", "gpay", "internet_acquiring"] = _path
       ),
       do: {:ok, block(block, update_operation: :new_endpoint)}

  defp process_block_title(
         "sender parameters" = title,
         block,
         block_parse_settings,
         true,
         [_, section, "internet_acquiring"] = _path
       ),
       do:
         process_block_title(title, block, block_parse_settings, true, [
           section,
           "internet_acquiring"
         ])

  defp process_block_title(
         "sender parameters",
         block,
         _block_parse_settings,
         true,
         [_, "internet_acquiring"] = _path
       ),
       do: {:ok, block(block, update_name: "sender")}

  defp process_block_title(
         "sender parameters" = title,
         block,
         block_parse_settings,
         true,
         ["transferring_to_card"] = _path
       ),
       do:
         process_block_title(title, block, block_parse_settings, true, [
           "checkout",
           "internet_acquiring"
         ])

  defp process_block_title(
         "sender parameters" = title,
         block,
         block_parse_settings,
         true,
         ["p2pdebit"] = _path
       ),
       do:
         process_block_title(title, block, block_parse_settings, true, [
           "checkout",
           "internet_acquiring"
         ])

  defp process_block_title(
         "regular payment parameters" = title,
         block,
         block_parse_settings,
         true,
         [_, section, "internet_acquiring"] = _path
       ),
       do:
         process_block_title(title, block, block_parse_settings, true, [
           section,
           "internet_acquiring"
         ])

  defp process_block_title(
         "regular payment parameters",
         block,
         _block_parse_settings,
         true,
         [_, "internet_acquiring"] = _path
       ),
       do: {:ok, block(block, update_name: "regular_payment")}

  defp process_block_title(
         "parameters for 1-click payment",
         block,
         _block_parse_settings,
         true,
         [_, "internet_acquiring"] = _path
       ),
       do: {:ok, block(block, update_name: "one_click_payment")}

  defp process_block_title(
         "parameters for tokenization within the visa cards enrollment hub (vceh)",
         block,
         _block_parse_settings,
         true,
         ["obtain", "tokens"] = _path
       ),
       do: {:ok, block(block, update_name: "vceh_tokenization")}

  defp process_block_title(
         "parameters for tokenization by card number",
         block,
         _block_parse_settings,
         true,
         ["obtain", "tokens"] = _path
       ),
       do: {:ok, block(block, update_name: "card_tokenization")}

  defp process_block_title("response parameters", block, _block_parse_settings, true, _path),
    do: {:ok, block(block, is_request: false, update_operation: :patch)}

  defp process_block_title(
         "parameters for transfer to the current account",
         block,
         _block_parse_settings,
         true,
         ["transferring_to_card"] = _path
       ),
       do: {:ok, block(block, update_name: "receiver_account")}

  defp process_block_title(
         "parameters for aggregators",
         block,
         _block_parse_settings,
         true,
         [_, "shop_create", "partnership"] = _path
       ),
       do: {:ok, block(block, update_name: "aggregator")}

  defp process_block_title(
         "api invoice_units",
         _block_data,
         _block_parse_settings,
         true,
         ["cancel", "invoice", "internet_acquiring"] = _path
       ),
       do: :error

  defp process_block_title(
         "payment widget parameters",
         _block_data,
         _block_parse_settings,
         true,
         _path
       ),
       do: :error

  defp process_block_title(
         "callback parameters",
         block,
         _block_parse_settings,
         true,
         ["register", "shop_create", "partnership"] = _path
       ),
       do: {:ok, block(block, is_request: false, update_operation: :patch)}

  defp process_block_title(
         "available мсс",
         block,
         _block_parse_settings,
         true,
         ["register", "shop_create", "partnership"] = _path
       ),
       do: {:ok, block(block, update_operation: :new_endpoint, update_name: "available_mcc")}

  defp process_block_title(
         "example answer",
         block,
         _block_parse_settings,
         true,
         [_, "shop_create", "partnership"] = _path
       ),
       do: {:ok, block(block, is_request: false, update_operation: :patch)}

  defp process_block_title(
         "documents",
         block,
         _block_parse_settings,
         true,
         ["available_mcc", "shop_create", "partnership"] = _path
       ),
       do: {:ok, block(block, update_operation: :new_endpoint, update_name: "documents")}

  defp process_block_title(
         "token obtainment",
         block(node: nil) = block,
         _block_parse_settings,
         true,
         ["tokens"] = _path
       ),
       do: {:ok, block(block, update_operation: :new_endpoint, update_name: "obtain")}

  defp process_block_title(
         "status change",
         block(node: nil) = block,
         _block_parse_settings,
         true,
         ["tokens"] = _path
       ),
       do: {:ok, block(block, update_operation: :new_endpoint, update_name: "change_status")}

  defp process_block_title(
         "compensation once a day",
         block(node: nil) = block,
         _block_parse_settings,
         true,
         ["register", "information"] = _path
       ),
       do:
         {:ok, block(block, update_operation: :new_endpoint, update_name: "compensation_per_day")}

  defp process_block_title(
         "compensation per transaction",
         block(node: nil) = block,
         _block_parse_settings,
         true,
         ["register", "information"] = _path
       ),
       do:
         {:ok,
          block(block,
            update_operation: :new_endpoint,
            update_name: "compensation_per_transaction"
          )}

  defp process_block_title(
         "getting the compensation registry",
         block(node: nil) = block,
         _block_parse_settings,
         true,
         ["register", "information"] = _path
       ),
       do:
         {:ok, block(block, update_operation: :new_endpoint, update_name: "compensation_report")}

  defp process_block_title(
         "parameters for generation of the first request",
         block,
         _block_parse_settings,
         true,
         ["compensation_report", "register", "information"] = _path
       ),
       do: {:ok, block(block, update_operation: :new_endpoint)}

  defp process_block_title(
         "parameters for generation of the second request",
         block,
         _block_parse_settings,
         true,
         ["compensation_report", "register", "information"] = _path
       ),
       do:
         {:ok,
          block(block, update_operation: :new_endpoint, update_name: "compensation_report_status")}

  defp process_block_title(
         "request statuses",
         _block_data,
         _block_parse_settings,
         true,
         ["compensation_report_status", "register", "information"] = _path
       ),
       do: :error

  defp process_block_title(
         "registry by p2p operation",
         block(node: nil) = block,
         _block_parse_settings,
         true,
         ["register", "information"]
       ),
       do:
         {:ok,
          block(block, update_operation: :new_endpoint, update_name: "compensation_report_p2p")}

  defp process_block_title(
         "parameters for generation of the request",
         block,
         _block_parse_settings,
         true,
         ["compensation_report_p2p", "register", "information"] = _path
       ),
       do: {:ok, block(block, update_operation: :new_endpoint)}

  defp process_block_title(
         "parameters for generation of the second request",
         block,
         _block_parse_settings,
         true,
         ["compensation_report_p2p", "register", "information"] = _path
       ),
       do:
         {:ok,
          block(block,
            update_operation: :new_endpoint,
            update_name: "compensation_report_p2p_status"
          )}

  defp process_block_title(
         "request statuses",
         _block_data,
         _block_parse_settings,
         true,
         ["compensation_report_p2p_status", "register", "information"] = _path
       ),
       do: :error

  defp update_block_schema(schema, _block_data, _block_parse_settings, true, _path) do
    {true, schema}
  end

  defp update_block_schema(
         %{type: type} = schema,
         block(update_type: type, update_name: nil) = block_data,
         block_parse_settings,
         false,
         path
       ) do
    schema_new = process_block_properties(schema, block_data, block_parse_settings, path)
    {true, schema_new}
  end

  defp update_block_schema(
         %{type: :object} = schema,
         block(update_operation: :patch, update_type: :object, update_name: "rro_info") =
           block_data,
         block_parse_settings,
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
      |> process_block_properties(block_data, block_parse_settings, path)

    {true, schema_new}
  end

  defp update_block_schema(
         %{type: type} = schema,
         block(update_operation: :patch, update_type: type, update_name: name) = block_data,
         block_parse_settings,
         false,
         [name | _] = path
       ) do
    path_new = if type == :array, do: [[] | path], else: path
    schema_new = process_block_properties(schema, block_data, block_parse_settings, path_new)
    {true, schema_new}
  end

  defp update_block_schema(
         %{type: type} = schema,
         block(update_operation: :new, update_type: type, update_name: name) = block_data,
         block_parse_settings,
         false,
         path
       )
       when is_binary(name) do
    schema_new =
      process_block_properties(schema, block_data, block_parse_settings, [name | path])

    {true, schema_new}
  end

  defp update_block_schema(
         %{type: :object, properties: properties} = schema,
         block_data,
         block_parse_settings,
         false,
         path
       ) do
    {properties_new, is_processed} =
      Enum.map_reduce(properties, false, fn {key, schema}, is_processed ->
        {is_processed_new, schema_new} =
          update_block_schema(schema, block_data, block_parse_settings, is_processed, [
            key | path
          ])

        {{key, schema_new}, is_processed_new}
      end)

    schema_new = %{schema | properties: OrderedObject.new(properties_new)}
    {is_processed, schema_new}
  end

  defp update_block_schema(
         %{type: :array, items: items} = schema,
         block_data,
         block_parse_settings,
         false,
         path
       ) do
    {is_processed, items_new} =
      update_block_schema(items, block_data, block_parse_settings, false, [[] | path])

    schema_new = %{schema | items: items_new}
    {is_processed, schema_new}
  end

  defp update_block_schema(
         schema,
         _block_data,
         _block_parse_settings,
         false,
         _path
       ) do
    {false, schema}
  end

  defp process_block_properties(
         schema,
         block(node: node, is_request: is_request) = block_data,
         block_parse_settings(
           table_classes: table_classes,
           table_standalone_code_block_class: table_standalone_code_block_class
         ) = block_parse_settings,
         path
       ) do
    table =
      table_classes
      |> Enum.find_value(fn class ->
        case Floki.find(node, "div.#{class}.MuiBox-root") do
          [table] -> table
          [] -> nil
        end
      end)

    is_full_property =
      is_request or
        table
        |> Floki.find(
          "table.MuiTable-root thead.MuiTableHead-root tr.MuiTableRow-root.MuiTableRow-head th.MuiTableCell-root.MuiTableCell-head"
        )
        |> Enum.at(1)
        |> parse_node_text(block_parse_settings)
        |> String.downcase()
        |> Kernel.==("required")

    {properties, required} =
      table
      |> Floki.find("table.MuiTable-root tbody.MuiTableBody-root tr.MuiTableRow-root")
      |> Enum.map(fn property ->
        [name, required, type, description | rest] =
          property
          |> Floki.find("td.MuiTableCell-root.MuiTableCell-body")
          |> case do
            [name, type, description | rest] when not is_full_property ->
              [name, {"div", [], ["Optional"]}, type, description | rest]

            full_property ->
              full_property
          end

        name = parse_property_name(name, block_parse_settings)
        description = parse_node_text(description, block_parse_settings)
        type = parse_property_type(type, block_parse_settings)
        required = parse_property_required(required, block_parse_settings)

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
      case block_data do
        block(update_operation: :new, update_name: name, description: description)
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

  defp parse_property_name(str, block_parse_settings) do
    str |> parse_node_text(block_parse_settings) |> String.trim()
  end

  defp parse_property_type(str, block_parse_settings) do
    str
    |> parse_node_text(block_parse_settings)
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

  defp parse_property_required(str, block_parse_settings) do
    str
    |> parse_node_text(block_parse_settings)
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

  defp search_code_blocks(
         {"div", _, _} = div,
         block_parse_settings(
           title_class: title_class,
           subtitle_classes: subtitle_classes,
           code_block_class: code_block_class,
           irrelevant_code_block_class: irrelevant_code_block_class
         ) = block_parse_settings,
         {title, subtitle, code_blocks},
         path
       ) do
    classes =
      div
      |> node_classes()
      |> MapSet.new()

    cond do
      MapSet.member?(classes, title_class) ->
        title_new = parse_block_title_text(div, block_parse_settings)
        {title_new, subtitle, code_blocks}

      not (classes
           |> MapSet.intersection(subtitle_classes)
           |> Enum.empty?()) ->
        subtitle_new = parse_block_title_text(div, block_parse_settings)
        {title, subtitle_new, code_blocks}

      MapSet.member?(classes, code_block_class) ->
        {title, subtitle}
        |> parse_block_title(block_parse_settings)
        |> downcase_block_title()
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
          &search_code_blocks(&1, block_parse_settings, &2, path)
        )
    end
  end

  defp search_code_blocks(_node, _block_parse_settings, acc, _path), do: acc

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

      ["goods", {:schema, :request}, "issue", "invoice", "internet_acquiring"] ->
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

  defp parse_standalone_example("response example", _div, [
         "decrypted_token",
         "gpay",
         "internet_acquiring"
       ]),
       do: :error

  defp parse_standalone_example("response example", div, path),
    do: parse_standalone_example(false, div, path)

  defp parse_standalone_example(
         "sample response for mastercard",
         div,
         ["obtain", "tokens"] = path
       ),
       do: parse_standalone_example(false, div, path)

  defp parse_standalone_example("sample response for visa", div, ["obtain", "tokens"] = path),
    do: parse_standalone_example(false, div, path)

  defp parse_standalone_example({"example response", _}, div, ["MPI", "confirmation"] = path),
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
                  ~r/liqpay\.(?:cnb_form\(|api\(\s*\"(?:\w+\/)*request\"\s*,)\s*(\{(?:[^}{]+|(?1))*+\})/,
                  &1,
                  capture: :all_but_first
                )
              )

            code_string
            |> String.replace(~r/(\{[^}{]+),(\s*\})/, "\\1\\2")
            |> Jason.decode!()
            |> then(&{:ok, {is_request, &1}})

          [] ->
            :error
        end
    end
  end

  defp parse_node_text([node], block_parse_settings),
    do: parse_node_text(node, block_parse_settings)

  defp parse_node_text(
         node,
         block_parse_settings(inline_code_text_class: inline_code_text_class) =
           block_parse_settings
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

        "[#{link |> parse_node_text(block_parse_settings) |> String.replace(~r/\s+/, " ") |> String.trim()}](#{@liqpay_base_url |> URI.merge(href) |> URI.to_string()})"

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
        |> parse_node_text(block_parse_settings)
        |> String.trim_trailing()
        |> Kernel.<>("\n")
    end)
    |> String.replace(~r/\n\s+([^[:upper:]`])/, " \\1")
    |> String.trim()
  end
end
