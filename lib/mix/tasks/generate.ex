defmodule Mix.Tasks.Generate do
  @moduledoc "Generates library's modules"
  use Mix.Task
  alias OpenAPIClient.Client.TypedDecoder
  alias Jason.OrderedObject

  @liqpay_base_url "https://www.liqpay.ua"
  @api_url "#{@liqpay_base_url}/en/doc/api"

  @internet_acquiring_regular_payment_fields ~w(subscribe subscribe_date_start subscribe_periodicity)
  @partnership_card_fields ~w(card card_cvv card_exp_month card_exp_year)

  @date_time_liqpay_format "date-time-liqpay"
  @date_liqpay_format "date-liqpay"
  @boolean_yesno_format "boolean-yesno"
  @boolean_integer_format "boolean-integer"
  @timestamp_ms_format "timestamp-ms"
  @month_year_liqpay_format "month-year-liqpay"

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
    code_block_language_panel_class: nil,
    code_block_language_panel_menu_class: nil,
    code_block_language_panel_menu_item_class: nil,
    irrelevant_code_block_class: nil,
    title_classes: MapSet.new(),
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
      |> Enum.map(fn
        section(id: id) -> id
        endpoint(id: id) -> id
      end)
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

        url_new
        |> process_url(parse_settings(parse_settings, path: [item | path]))
        |> case do
          {:ok, children} -> section(item, children: children)
          :error -> item
        end
      end)
      |> then(&{:ok, &1})
    else
      _ -> :error
    end
  end

  defp parse_doc_page(parse_settings(document: document, path: path) = parse_settings) do
    document
    |> Floki.find("div.base-TabsList-root")
    |> case do
      [tab] -> parse_tab(tab, parse_settings)
      [] -> process_doc(parse_settings)
    end
    |> case do
      :error ->
        :error

      {:ok, endpoints} = result ->
        case path do
          [section(type: :menu) | _] ->
            path_ids = Enum.map(path, fn section(id: id) -> id end)

            json_file =
              path_ids
              |> List.update_at(0, &"#{&1}.json")
              |> Enum.reverse()
              |> then(&["specs" | &1])
              |> Path.join()

            json_file
            |> Path.dirname()
            |> File.mkdir_p!()

            endpoint_path = if Enum.count(endpoints) == 1, do: tl(path_ids), else: path_ids

            OrderedObject.new(
              openapi: "3.1.0",
              info: OrderedObject.new(version: "3", title: "External API"),
              servers: [OrderedObject.new(url: "https://liqpay.ua")],
              paths:
                endpoints
                |> Enum.map(fn endpoint(
                                 id: id,
                                 request: request_schema,
                                 response: response_schema
                               ) ->
                  url =
                    [id | endpoint_path]
                    |> Enum.reverse()
                    |> then(&["/" | &1])
                    |> Path.join()

                  {url,
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
                                     OrderedObject.new(
                                       schema: response_schema || %{type: :object}
                                     )}
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

          _ ->
            :ok
        end

        result
    end
  end

  defp parse_tab(tab, parse_settings(url: url, body: body, path: path) = parse_settings) do
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
        process_doc(parse_settings)

      :else ->
        results =
          Enum.flat_map(
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

              tab_item = section(type: :doc, title: tab_label, id: tab_id, url: url)

              {:ok, result} =
                if tab_index == 1 do
                  parse_settings
                  |> parse_settings(path: [tab_item | path])
                  |> process_doc()
                else
                  query_new =
                    decoded_query
                    |> Map.put("tab", Integer.to_string(tab_index))
                    |> URI.encode_query()

                  url_new = %URI{uri | query: query_new} |> URI.to_string()

                  tab_item_new = section(tab_item, url: url_new)
                  parse_settings_new = parse_settings(path: [tab_item_new | path])
                  process_url(url_new, parse_settings_new)
                end

              result
            end
          )

        {:ok, results}
    end
  end

  defp process_doc(
         parse_settings(path: [section(id: "widgets"), section(id: "internet_acquiring")])
       ),
       do: :error

  defp process_doc(
         parse_settings(path: [section(id: "splitting"), section(id: "internet_acquiring")])
       ),
       do: :error

  defp process_doc(parse_settings(path: [section(id: "testing")])), do: :error
  defp process_doc(parse_settings(path: [section(id: "errors")])), do: :error

  defp process_doc(parse_settings(document: document, path: path) = parse_settings) do
    main_block_selector =
      "new_doc_page_doc"
      |> find_class(parse_settings)
      |> then(&{&1, path})
      |> case do
        {class, [_, section(id: "public")]} when is_binary(class) ->
          "div.#{class} > div.MuiBox-root > div.MuiBox-root"

        {class, _path} when is_binary(class) ->
          "div.#{class} > div.MuiBox-root > div.MuiBox-root > div.MuiBox-root"

        {nil, [section(id: "callback")]} ->
          class = find_class!("new_doc_doc_container_child", parse_settings)
          "div.#{class} > div.MuiBox-root > div.MuiBox-root > div.MuiBox-root"
      end

    title_classes =
      "new_doc_(?:integration|page)_titles"
      |> find_classes(parse_settings)
      |> MapSet.new()

    subtitle_classes =
      "(?:new_doc_possibilities_(?:text|subtitle)|doc_page_index_indent)"
      |> find_classes(parse_settings)
      |> MapSet.new()

    inline_code_text_class = find_class("new_doc_integration_code_text", parse_settings)

    table_classes =
      "new_doc_table_scroll(?:_registry)?(?:_\\w+)?"
      |> find_classes(parse_settings)
      |> MapSet.new()

    table_standalone_code_block_class = find_class("new_doc_table_code", parse_settings)
    standalone_code_block_class = find_class("new_doc_page_content", parse_settings)
    code_block_class = find_class("doc_code_style", parse_settings)

    code_block_language_panel_class =
      code_block_class && find_class("doc_doc_panel_menu", parse_settings)

    code_block_language_panel_menu_class =
      code_block_language_panel_class && find_class("doc_panel_mob", parse_settings)

    code_block_language_panel_menu_item_class =
      code_block_language_panel_menu_class && find_class("doc_doc_panel_menu_a", parse_settings)

    irrelevant_code_block_class = find_class("new_doc_green_page_background", parse_settings)

    document
    |> Floki.find(main_block_selector)
    |> parse_doc(
      block_parse_settings(
        inline_code_text_class: inline_code_text_class,
        table_classes: table_classes,
        table_standalone_code_block_class: table_standalone_code_block_class,
        standalone_code_block_class: standalone_code_block_class,
        code_block_class: code_block_class,
        code_block_language_panel_class: code_block_language_panel_class,
        code_block_language_panel_menu_class: code_block_language_panel_menu_class,
        code_block_language_panel_menu_item_class: code_block_language_panel_menu_item_class,
        irrelevant_code_block_class: irrelevant_code_block_class,
        title_classes: title_classes,
        subtitle_classes: subtitle_classes
      ),
      path
    )
  end

  defp find_class_ensure_regex(class_prefix) when is_binary(class_prefix) do
    ~s'(?<!\\w)#{class_prefix}__\\w+(?!\\w)'
    |> Regex.compile!()
    |> find_class_ensure_regex()
  end

  defp find_class_ensure_regex(%Regex{} = regex), do: regex

  defp find_classes(pattern, parse_settings(body: body)) do
    pattern
    |> find_class_ensure_regex()
    |> Regex.scan(body, capture: :first)
    |> Enum.map(fn [class] -> class end)
    |> Enum.uniq()
  end

  defp find_class(pattern, parse_settings) do
    pattern
    |> find_classes(parse_settings)
    |> case do
      [class] -> class
      [] -> nil
    end
  end

  defp find_class!(pattern, parse_settings) do
    [class] = find_classes(pattern, parse_settings)
    class
  end

  defp parse_doc(
         block_nodes,
         block_parse_settings(
           standalone_code_block_class: standalone_code_block_class,
           table_classes: table_classes
         ) = block_parse_settings,
         path
       ) do
    {endpoints, code_blocks, _path} =
      Enum.reduce(block_nodes, {[], [], path}, fn node, {endpoints, code_blocks, path} ->
        classes = node_classes(node)

        cond do
          standalone_code_block_class && Enum.member?(classes, standalone_code_block_class) ->
            node
            |> parse_block_title(block_parse_settings)
            |> downcase_block_title()
            |> parse_standalone_example(node, block_parse_settings, path)
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
              {:ok, block_data} ->
                process_block_data(
                  {endpoints, code_blocks, path},
                  block_data,
                  block_parse_settings
                )

              :error ->
                {endpoints, code_blocks, path}
            end

          :else ->
            {_, _, code_blocks_new} =
              search_code_blocks(node, block_parse_settings, {nil, nil, code_blocks}, path)

            {endpoints, code_blocks_new, path}
        end
      end)

    endpoints
    |> List.update_at(0, &process_endpoint_code_blocks(&1, code_blocks, path))
    |> Enum.reverse()
    |> then(&{:ok, &1})
  end

  defp process_block_data(
         {endpoints, code_blocks, [_section | rest_path] = path},
         block(update_operation: :new_endpoint, update_name: "units" = id) = block_data,
         block_parse_settings
       ) do
    endpoint = endpoint(id: id)
    endpoints_new = [endpoint | endpoints]
    path_new = [endpoint | rest_path]

    block_data_new =
      block(block_data,
        is_request: true,
        update_operation: :patch,
        update_name: nil
      )

    {[top_endpoint, previous_endpoint | rest_endpoints], code_blocks_new, _path_new} =
      process_block_data(
        {endpoints_new, code_blocks, path_new},
        block_data_new,
        block_parse_settings
      )

    top_endpoint_new =
      endpoint(top_endpoint,
        response: %{
          oneOf: [
            %{
              type: :object,
              properties:
                OrderedObject.new([
                  {"id", %{type: :string, description: "unique number"}},
                  {"full_name_uk", %{type: :string, description: "full name in Ukrainian"}},
                  {"full_name_en", %{type: :string, description: "full name in English"}},
                  {"short_name_uk", %{type: :string, description: "short name in Ukrainian"}},
                  {"short_name_en", %{type: :string, description: "short name in English"}}
                ])
            },
            %{
              type: :object,
              properties:
                OrderedObject.new([
                  {"id", %{type: :string, description: "unique number"}},
                  {"full_name", %{type: :string, description: "full name"}},
                  {"short_name", %{type: :string, description: "short name"}}
                ])
            }
          ]
        }
      )

    endpoints_new = [previous_endpoint, top_endpoint_new | rest_endpoints]
    {endpoints_new, code_blocks_new, path}
  end

  defp process_block_data(
         {[] = endpoints, code_blocks, [section(id: id, title: title) | rest_path] = _path},
         block(update_operation: :new_endpoint) = block_data,
         block_parse_settings
       ) do
    endpoint = endpoint(id: id, title: title)
    endpoints_new = [endpoint | endpoints]
    path_new = [endpoint | rest_path]

    block_data_new =
      block(block_data,
        is_request: true,
        update_operation: :patch,
        update_name: nil
      )

    process_block_data(
      {endpoints_new, code_blocks, path_new},
      block_data_new,
      block_parse_settings
    )
  end

  defp process_block_data(
         {endpoints, code_blocks, [_section | rest_path] = path},
         block(update_operation: :new_endpoint, update_name: id) = block_data,
         block_parse_settings
       ) do
    endpoint = endpoint(id: id)
    endpoints_new = [endpoint | endpoints]
    path_new = [endpoint | rest_path]

    block_data_new =
      block(block_data,
        is_request: true,
        update_operation: :patch,
        update_name: nil
      )

    {[top_endpoint, previous_endpoint | rest_endpoints], code_blocks_new, path_new} =
      process_block_data(
        {endpoints_new, code_blocks, path_new},
        block_data_new,
        block_parse_settings
      )

    previous_endpoint_new =
      process_endpoint_code_blocks(previous_endpoint, code_blocks_new, path)

    endpoints_new = [top_endpoint, previous_endpoint_new | rest_endpoints]
    {endpoints_new, [], path_new}
  end

  defp process_block_data(
         {[top_endpoint | rest_endpoints], code_blocks, path},
         block_data,
         block_parse_settings
       ) do
    top_endpoint_new =
      update_block_endpoint(
        top_endpoint,
        block_data,
        block_parse_settings,
        path
      )

    endpoints_new = [top_endpoint_new | rest_endpoints]
    {endpoints_new, code_blocks, path}
  end

  defp update_block_endpoint(
         endpoint,
         block(is_request: true, update_operation: :patch, update_type: :object, update_name: nil) =
           block_data,
         block_parse_settings,
         [
           endpoint(id: "compensation_report"),
           section(id: "register"),
           section(id: "information")
         ] = path
       ) do
    endpoint
    |> do_update_block_endpoint(block_data, block_parse_settings, path)
    |> case do
      endpoint(response: nil) = endpoint_new ->
        response = %{
          type: :object,
          required: ["register_token", "result", "status"],
          properties:
            OrderedObject.new([
              {"register_token",
               %{type: :string, description: "Parameter received on the previous request"}},
              {"result", %{type: :string, description: "The result of the request"}},
              {"status", %{type: :string, description: "Request processing status"}}
            ])
        }

        endpoint(endpoint_new, response: response)

      endpoint_new ->
        endpoint_new
    end
  end

  defp update_block_endpoint(endpoint, block_data, block_parse_settings, path) do
    do_update_block_endpoint(endpoint, block_data, block_parse_settings, path)
  end

  defp do_update_block_endpoint(
         endpoint,
         block(is_request: is_request) = block_data,
         block_parse_settings,
         path
       ) do
    {block_schema_type, block_schema} =
      if is_request do
        endpoint(request: request_schema) = endpoint
        {:request, request_schema}
      else
        endpoint(response: response_schema) = endpoint
        {:response, response_schema}
      end

    block_schema_new =
      (block_schema || %{type: :object, properties: OrderedObject.new([])})
      |> update_block_schema(block_data, block_parse_settings, false, [
        {:schema, block_schema_type} | path
      ])
      |> case do
        {true, block_schema_new} ->
          block_schema_new

        {false, _block_schema_new} ->
          IO.inspect(block_data |> block() |> Keyword.drop([:node]),
            label: "Unprocessed block (#{loggable_schema_path(path)})"
          )
      end

    if is_request,
      do: endpoint(endpoint, request: block_schema_new),
      else: endpoint(endpoint, response: block_schema_new)
  end

  defp process_endpoint_code_blocks(endpoint, code_blocks, path) do
    code_blocks
    |> Enum.reverse()
    |> Enum.reduce(endpoint, fn
      {is_request, example}, endpoint ->
        path_new =
          case path do
            [section(type: :doc) | rest_path] -> [endpoint | rest_path]
            _ -> path
          end

        {schema_type, schema} =
          if is_request do
            endpoint(request: request_schema) = endpoint
            {:request, request_schema}
          else
            endpoint(response: response_schema) = endpoint
            {:response, response_schema}
          end

        schema_new = patch_schema_examples(example, schema, [{:schema, schema_type} | path_new])

        if is_request,
          do: endpoint(endpoint, request: schema_new),
          else: endpoint(endpoint, response: schema_new)
    end)
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
           title_classes: title_classes,
           subtitle_classes: subtitle_classes
         ) = block_parse_settings
       ) do
    title = parse_block_title(node, block_parse_settings, title_classes)
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

  defp process_block_title(title, block_data, block_parse_settings, false, path)
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
         block(block_data,
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
          block(block_data, description: title),
          block_parse_settings,
          true,
          path
        )
    end
  end

  defp process_block_title({title, subtitle}, block_data, block_parse_settings, false, path)
       when is_binary(title) and is_binary(subtitle) do
    {title, subtitle}
    |> downcase_block_title()
    |> process_block_title(
      block(block_data, description: title),
      block_parse_settings,
      true,
      path
    )
  end

  defp process_block_title(
         "encrypted token",
         block(node: nil) = block_data,
         _block_parse_settings,
         true,
         [section(id: "apay"), section(id: "internet_acquiring")] = _path
       ),
       do:
         {:ok, block(block_data, update_operation: :new_endpoint, update_name: "encrypted_token")}

  defp process_block_title(
         "decrypted token",
         block(node: nil) = block_data,
         _block_parse_settings,
         true,
         [section(id: "apay"), section(id: "internet_acquiring")] = _path
       ),
       do:
         {:ok, block(block_data, update_operation: :new_endpoint, update_name: "decrypted_token")}

  defp process_block_title(
         "encrypted token",
         block(node: nil) = block_data,
         _block_parse_settings,
         true,
         [section(id: "gpay"), section(id: "internet_acquiring")] = _path
       ),
       do:
         {:ok, block(block_data, update_operation: :new_endpoint, update_name: "encrypted_token")}

  defp process_block_title(
         "decrypted token",
         block(node: nil) = block_data,
         _block_parse_settings,
         true,
         [section(id: "gpay"), section(id: "internet_acquiring")] = _path
       ),
       do:
         {:ok, block(block_data, update_operation: :new_endpoint, update_name: "decrypted_token")}

  defp process_block_title(
         "create subscribtion",
         block(node: nil) = block_data,
         _block_parse_settings,
         true,
         [section(id: "subscription"), section(id: "internet_acquiring")] = _path
       ),
       do: {:ok, block(block_data, update_operation: :new_endpoint, update_name: "create")}

  defp process_block_title(
         "unsubscribe",
         block(node: nil) = block_data,
         _block_parse_settings,
         true,
         [
           section(id: "subscription"),
           section(id: "internet_acquiring")
         ] = _path
       ),
       do: {:ok, block(block_data, update_operation: :new_endpoint, update_name: "unsubscribe")}

  defp process_block_title(
         "edit subscribtion",
         block(node: nil) = block_data,
         _block_parse_settings,
         true,
         [section(id: "subscription"), section(id: "internet_acquiring")] = _path
       ),
       do: {:ok, block(block_data, update_operation: :new_endpoint, update_name: "edit")}

  defp process_block_title(
         "funds blocking",
         block(node: nil) = block_data,
         _block_parse_settings,
         true,
         [section(id: "two_step"), section(id: "internet_acquiring")] = _path
       ),
       do: {:ok, block(block_data, update_operation: :new_endpoint, update_name: "block")}

  defp process_block_title(
         "completion of payment",
         block(node: nil) = block_data,
         _block_parse_settings,
         true,
         [section(id: "two_step"), section(id: "internet_acquiring")] = _path
       ),
       do: {:ok, block(block_data, update_operation: :new_endpoint, update_name: "complete")}

  defp process_block_title(
         "issuing the invoice",
         block(node: nil) = block_data,
         _block_parse_settings,
         true,
         [section(id: "invoice"), section(id: "internet_acquiring")] = _path
       ),
       do: {:ok, block(block_data, update_operation: :new_endpoint, update_name: "issue")}

  defp process_block_title(
         "invoice cancelation",
         block(node: nil) = block_data,
         _block_parse_settings,
         true,
         [section(id: "invoice"), section(id: "internet_acquiring")] = _path
       ),
       do: {:ok, block(block_data, update_operation: :new_endpoint, update_name: "cancel")}

  defp process_block_title(
         "company creation create",
         block(node: nil) = block_data,
         _block_parse_settings,
         true,
         [section(id: "shop_create"), section(id: "partnership")] = _path
       ),
       do: {:ok, block(block_data, update_operation: :new_endpoint, update_name: "create")}

  defp process_block_title(
         "company creation register",
         block(node: nil) = block_data,
         _block_parse_settings,
         true,
         [section(id: "shop_create"), section(id: "partnership")] = _path
       ),
       do: {:ok, block(block_data, update_operation: :new_endpoint, update_name: "register")}

  defp process_block_title("main", block_data, _block_parse_settings, true, _path),
    do: {:ok, block(block_data, update_operation: :new_endpoint)}

  defp process_block_title(
         "other parameters" = title,
         block_data,
         block_parse_settings,
         true,
         [_ | [_, section(id: "internet_acquiring")] = rest_path] = _path
       ),
       do: process_block_title(title, block_data, block_parse_settings, true, rest_path)

  defp process_block_title(
         "other parameters",
         block_data,
         _block_parse_settings,
         true,
         [_, section(id: "internet_acquiring")] = _path
       ),
       do: {:ok, block(block_data, update_operation: :patch)}

  defp process_block_title(
         "other parameters" = title,
         block_data,
         block_parse_settings,
         true,
         [endpoint(id: "transferring_to_card")] = _path
       ),
       do:
         process_block_title(title, block_data, block_parse_settings, true, [
           endpoint(id: "checkout"),
           section(id: "internet_acquiring")
         ])

  defp process_block_title(
         "parameters of splitting the payments" = title,
         block_data,
         block_parse_settings,
         true,
         [_ | [_, section(id: "internet_acquiring")] = rest_path] = _path
       ),
       do: process_block_title(title, block_data, block_parse_settings, true, rest_path)

  defp process_block_title(
         "parameters of splitting the payments",
         block_data,
         _block_parse_settings,
         true,
         [_, section(id: "internet_acquiring")] = _path
       ),
       do: {:ok, block(block_data, update_operation: :patch)}

  defp process_block_title(
         "parameters for tokenization within the token connect control",
         block_data,
         _block_parse_settings,
         true,
         [endpoint(id: "obtain"), section(id: "tokens")] = _path
       ),
       do: {:ok, block(block_data, update_operation: :patch)}

  defp process_block_title(
         "parameters for transfer to the card",
         block_data,
         _block_parse_settings,
         true,
         [endpoint(id: "transferring_to_card")] = _path
       ),
       do: {:ok, block(block_data, update_operation: :patch)}

  defp process_block_title(
         "parameters for transfer to the card's token",
         block_data,
         _block_parse_settings,
         true,
         [endpoint(id: "transferring_to_card")] = _path
       ),
       do: {:ok, block(block_data, update_operation: :patch)}

  defp process_block_title(
         "receiver parameters",
         block_data,
         _block_parse_settings,
         true,
         [endpoint(id: "transferring_to_card")] = _path
       ),
       do: {:ok, block(block_data, update_operation: :patch)}

  defp process_block_title(
         "parameters for data formation",
         block_data,
         _block_parse_settings,
         true,
         [section(id: "decrypted_token"), section(id: "gpay"), section(id: "internet_acquiring")] =
           _path
       ),
       do: {:ok, block(block_data, update_operation: :new_endpoint)}

  defp process_block_title(
         "sender parameters" = title,
         block_data,
         block_parse_settings,
         true,
         [_ | [_, section(id: "internet_acquiring")] = rest_path] = _path
       ),
       do: process_block_title(title, block_data, block_parse_settings, true, rest_path)

  defp process_block_title(
         "sender parameters",
         block_data,
         _block_parse_settings,
         true,
         [_, section(id: "internet_acquiring")] = _path
       ),
       do: {:ok, block(block_data, update_name: "sender")}

  defp process_block_title(
         "sender parameters" = title,
         block_data,
         block_parse_settings,
         true,
         [endpoint(id: "transferring_to_card")] = _path
       ),
       do:
         process_block_title(title, block_data, block_parse_settings, true, [
           section(id: "checkout"),
           section(id: "internet_acquiring")
         ])

  defp process_block_title(
         "sender parameters" = title,
         block_data,
         block_parse_settings,
         true,
         [endpoint(id: "p2pdebit")] = _path
       ),
       do:
         process_block_title(title, block_data, block_parse_settings, true, [
           section(id: "checkout"),
           section(id: "internet_acquiring")
         ])

  defp process_block_title(
         "regular payment parameters" = title,
         block_data,
         block_parse_settings,
         true,
         [_ | [_, section(id: "internet_acquiring")] = rest_path] = _path
       ),
       do: process_block_title(title, block_data, block_parse_settings, true, rest_path)

  defp process_block_title(
         "regular payment parameters",
         block_data,
         _block_parse_settings,
         true,
         [_, section(id: "internet_acquiring")] = _path
       ),
       do: {:ok, block(block_data, update_name: "regular_payment")}

  defp process_block_title(
         "parameters for 1-click payment",
         block_data,
         _block_parse_settings,
         true,
         [_, section(id: "internet_acquiring")] = _path
       ),
       do: {:ok, block(block_data, update_name: "one_click_payment")}

  defp process_block_title(
         "parameters for tokenization within the visa cards enrollment hub (vceh)",
         block_data,
         _block_parse_settings,
         true,
         [endpoint(id: "obtain"), section(id: "tokens")] = _path
       ),
       do: {:ok, block(block_data, update_name: "vceh_tokenization")}

  defp process_block_title(
         "parameters for tokenization by card number",
         block_data,
         _block_parse_settings,
         true,
         [endpoint(id: "obtain"), section(id: "tokens")] = _path
       ),
       do: {:ok, block(block_data, update_name: "card_tokenization")}

  defp process_block_title(
         "response parameters",
         block_data,
         _block_parse_settings,
         true,
         [section(id: "callback")] = _path
       ),
       do: {:ok, block(block_data, update_operation: :new_endpoint)}

  defp process_block_title("response parameters", block_data, _block_parse_settings, true, _path),
    do: {:ok, block(block_data, is_request: false, update_operation: :patch)}

  defp process_block_title(
         "parameters for transfer to the current account",
         block_data,
         _block_parse_settings,
         true,
         [endpoint(id: "transferring_to_card")] = _path
       ),
       do: {:ok, block(block_data, update_name: "receiver_account")}

  defp process_block_title(
         "parameters for aggregators",
         block_data,
         _block_parse_settings,
         true,
         [_, section(id: "shop_create"), section(id: "partnership")] = _path
       ),
       do: {:ok, block(block_data, update_name: "aggregator")}

  defp process_block_title(
         "api invoice_units",
         block_data,
         _block_parse_settings,
         true,
         [endpoint(id: "issue"), section(id: "invoice"), section(id: "internet_acquiring")] =
           _path
       ),
       do: {:ok, block(block_data, update_operation: :new_endpoint, update_name: "units")}

  defp process_block_title(
         "payment widget parameters",
         _block_data,
         _block_parse_settings,
         true,
         [section(id: "widget"), section(id: "internet_acquiring")] = _path
       ),
       do: :error

  defp process_block_title(
         "callback parameters",
         block_data,
         _block_parse_settings,
         true,
         [endpoint(id: "register"), section(id: "shop_create"), section(id: "partnership")] =
           _path
       ),
       do: {:ok, block(block_data, is_request: false, update_operation: :patch)}

  defp process_block_title(
         "available мсс",
         block_data,
         _block_parse_settings,
         true,
         [endpoint(id: "register"), section(id: "shop_create"), section(id: "partnership")] =
           _path
       ),
       do: {:ok, block(block_data, update_operation: :new_endpoint, update_name: "available_mcc")}

  defp process_block_title(
         "example answer",
         block_data,
         _block_parse_settings,
         true,
         [_, section(id: "shop_create"), section(id: "partnership")] = _path
       ),
       do: {:ok, block(block_data, is_request: false, update_operation: :patch)}

  defp process_block_title(
         "documents",
         block_data,
         _block_parse_settings,
         true,
         [endpoint(id: "available_mcc"), section(id: "shop_create"), section(id: "partnership")] =
           _path
       ),
       do: {:ok, block(block_data, update_operation: :new_endpoint, update_name: "documents")}

  defp process_block_title(
         "token obtainment",
         block(node: nil) = block_data,
         _block_parse_settings,
         true,
         [section(id: "tokens")] = _path
       ),
       do: {:ok, block(block_data, update_operation: :new_endpoint, update_name: "obtain")}

  defp process_block_title(
         "status change",
         block(node: nil) = block_data,
         _block_parse_settings,
         true,
         [section(id: "tokens")] = _path
       ),
       do: {:ok, block(block_data, update_operation: :new_endpoint, update_name: "change_status")}

  defp process_block_title(
         "compensation once a day",
         block(node: nil) = block_data,
         _block_parse_settings,
         true,
         [section(id: "register"), section(id: "information")] = _path
       ),
       do:
         {:ok,
          block(block_data, update_operation: :new_endpoint, update_name: "compensation_per_day")}

  defp process_block_title(
         "compensation per transaction",
         block(node: nil) = block_data,
         _block_parse_settings,
         true,
         [section(id: "register"), section(id: "information")] = _path
       ),
       do:
         {:ok,
          block(block_data,
            update_operation: :new_endpoint,
            update_name: "compensation_per_transaction"
          )}

  defp process_block_title(
         "getting the compensation registry",
         block(node: nil) = block_data,
         _block_parse_settings,
         true,
         [section(id: "register"), section(id: "information")] = _path
       ),
       do:
         {:ok,
          block(block_data, update_operation: :new_endpoint, update_name: "compensation_report")}

  defp process_block_title(
         "parameters for generation of the first request",
         block_data,
         _block_parse_settings,
         true,
         [section(id: "compensation_report"), section(id: "register"), section(id: "information")] =
           _path
       ),
       do: {:ok, block(block_data, update_operation: :new_endpoint)}

  defp process_block_title(
         "parameters for generation of the second request",
         block_data,
         _block_parse_settings,
         true,
         [
           endpoint(id: "compensation_report"),
           section(id: "register"),
           section(id: "information")
         ] = _path
       ),
       do:
         {:ok,
          block(block_data,
            update_operation: :new_endpoint,
            update_name: "compensation_report_status"
          )}

  defp process_block_title(
         "request statuses",
         block_data,
         _block_parse_settings,
         true,
         [
           endpoint(id: endpoint_id),
           section(id: "register"),
           section(id: "information")
         ] = _path
       )
       when endpoint_id in ~w(compensation_report_status compensation_report_p2p_status),
       do:
         {:ok,
          block(block_data,
            is_request: false,
            update_operation: :patch,
            update_name: "response_statuses"
          )}

  defp process_block_title(
         "registry by p2p operation",
         block(node: nil) = block_data,
         _block_parse_settings,
         true,
         [section(id: "register"), section(id: "information")]
       ),
       do:
         {:ok,
          block(block_data,
            update_operation: :new_endpoint,
            update_name: "compensation_report_p2p"
          )}

  defp process_block_title(
         "parameters for generation of the request",
         block_data,
         _block_parse_settings,
         true,
         [
           section(id: "compensation_report_p2p"),
           section(id: "register"),
           section(id: "information")
         ] = _path
       ),
       do: {:ok, block(block_data, update_operation: :new_endpoint)}

  defp process_block_title(
         "parameters for generation of the second request",
         block_data,
         _block_parse_settings,
         true,
         [
           endpoint(id: "compensation_report_p2p"),
           section(id: "register"),
           section(id: "information")
         ] = _path
       ),
       do:
         {:ok,
          block(block_data,
            update_operation: :new_endpoint,
            update_name: "compensation_report_p2p_status"
          )}

  defp process_block_title(
         {"request parameters", "main"},
         block_data,
         _block_parse_settings,
         true,
         [_, section(id: "public")] = _path
       ),
       do: {:ok, block(block_data, update_operation: :new_endpoint)}

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
    schema_new = parse_block_schema(schema, block_data, block_parse_settings, path)
    {true, schema_new}
  end

  defp update_block_schema(
         %{type: :object} = schema,
         block(
           node: node,
           update_operation: :patch,
           update_type: :object,
           update_name: "response_statuses"
         ) = _block_data,
         block_parse_settings(table_classes: table_classes) = block_parse_settings,
         false,
         [
           {:schema, :response},
           endpoint(id: endpoint_id),
           section(id: "register"),
           section(id: "information")
         ] = _path
       )
       when endpoint_id in ~w(compensation_report_status compensation_report_p2p_status) do
    schema
    |> case do
      %{properties: properties} -> Enum.empty?(properties)
      _ -> true
    end
    |> if do
      table =
        table_classes
        |> Enum.find_value(fn class ->
          case Floki.find(node, "div.#{class}.MuiBox-root") do
            [table] -> table
            [] -> nil
          end
        end)

      statuses =
        table
        |> Floki.find("table.MuiTable-root tbody.MuiTableBody-root tr.MuiTableRow-root")
        |> Enum.map(fn row ->
          [name, desciption] = Floki.find(row, "td.MuiTableCell-root.MuiTableCell-body")
          name = parse_property_name(name, block_parse_settings)
          description = parse_node_text(desciption, block_parse_settings)
          {name, description}
        end)
        |> List.keydelete("result", 0)

      properties =
        OrderedObject.new([
          {"filelink", %{type: :string, format: :uri, description: "File URL"}},
          {"result",
           %{
             type: :string,
             description: "The result of a request",
             enum: ["ok", "error"]
           }},
          {"status",
           %{
             type: :string,
             description:
               "The status of a request. Possible values:#{Enum.map_join(statuses, fn {key, description} -> "\n* `#{key}` - #{description}" end)}",
             enum: Enum.map(statuses, fn {key, _description} -> key end)
           }}
        ])

      schema_new = Map.put(schema, :properties, properties)
      {true, schema_new}
    else
      {true, schema}
    end
  end

  defp update_block_schema(
         %{type: type} = schema,
         block(update_operation: :patch, update_type: type, update_name: name) = block_data,
         block_parse_settings,
         false,
         [name | _] = path
       ) do
    path_new = if type == :array, do: [[] | path], else: path
    schema_new = parse_block_schema(schema, block_data, block_parse_settings, path_new)
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
      parse_block_schema(schema, block_data, block_parse_settings, [name | path])

    {true, schema_new}
  end

  defp update_block_schema(
         %{type: :object, properties: properties} = schema,
         block(update_operation: :patch, update_type: :object, update_name: "rro_info" = name) =
           block_data,
         block_parse_settings,
         false,
         [{:schema, :request}, endpoint(id: "card_payment"), section(id: "internet_acquiring")] =
           path
       ) do
    {true, property_new} =
      properties
      |> ensure_block_properties([
        {name,
         %{
           type: :object,
           description: "Data for fiscalization",
           properties: OrderedObject.new([])
         }}
      ])
      |> get_in([name])
      |> update_block_schema(
        block_data,
        block_parse_settings,
        false,
        [name | path]
      )

    properties_new = ensure_block_properties(properties, [{name, property_new}])
    schema_new = %{schema | properties: properties_new}
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

  defp parse_block_schema(
         schema,
         block(update_operation: :patch, update_type: :object, update_name: nil) = block_data,
         block_parse_settings,
         [
           {:schema, :response},
           endpoint(id: "compensation_per_day"),
           section(id: "register"),
           section(id: "information")
         ] =
           path
       ) do
    schema_new =
      do_parse_block_schema(schema, block_data, block_parse_settings, path)

    schema
    |> case do
      %{properties: properties} -> Enum.empty?(properties)
      _ -> true
    end
    |> if do
      update_in(schema_new, [:properties, "data", :items], fn %{type: :object} = items_schema ->
        %{
          oneOf: [
            items_schema,
            %{
              type: :object,
              properties:
                OrderedObject.new([
                  {"compensation_id",
                   %{
                     type: :string,
                     description: "compensation_id of enrollment"
                   }},
                  {"create_date", %{type: :string, format: @date_time_liqpay_format}}
                ])
            }
          ]
        }
      end)
    else
      schema_new
    end
  end

  defp parse_block_schema(
         schema,
         block(update_operation: :patch, update_type: :object, update_name: nil) = block_data,
         block_parse_settings,
         [
           {:schema, :response},
           endpoint(id: "exchange"),
           section(id: "public")
         ] =
           path
       ) do
    was_empty =
      case schema do
        %{properties: properties} -> Enum.empty?(properties)
        _ -> true
      end

    schema_new =
      do_parse_block_schema(
        schema,
        block_data,
        block_parse_settings,
        if(was_empty, do: [[] | path], else: path)
      )

    if was_empty do
      %{type: :array, items: schema_new}
    else
      schema_new
    end
  end

  defp parse_block_schema(
         schema,
         block(update_operation: :patch, update_type: :object, update_name: nil) = block_data,
         block_parse_settings,
         [
           {:schema, :response},
           endpoint(id: "archive"),
           section(id: "public")
         ] =
           path
       ) do
    was_empty =
      case schema do
        %{properties: properties} -> Enum.empty?(properties)
        _ -> true
      end

    schema_new =
      do_parse_block_schema(
        schema,
        block_data,
        block_parse_settings,
        if(was_empty, do: [[], "exchangeRate" | path], else: path)
      )

    if was_empty do
      %{properties: properties} = schema_new

      properties_new =
        Enum.flat_map(
          properties,
          fn
            {"saleRateNB/purchaseRateNB", property_schema} ->
              [
                {"saleRateNB", property_schema},
                {"purchaseRateNB",
                 Map.put(property_schema, :description, "The purchase rate of NBU")}
              ]

            other ->
              [other]
          end
        )
        |> OrderedObject.new()

      schema_new = %{schema_new | properties: properties_new}

      %{
        type: :object,
        properties:
          OrderedObject.new([
            {"date", %{type: :string, format: @date_liqpay_format}},
            {"bank", %{type: :string}},
            {"baseCurrency", %{type: :integer}},
            {"baseCurrencyLit", %{type: :string}},
            {"exchangeRate", %{type: :array, items: schema_new}}
          ])
      }
    else
      schema_new
    end
  end

  defp parse_block_schema(
         schema,
         block(update_operation: :patch, update_type: :object, update_name: nil) = block_data,
         block_parse_settings,
         [
           {:schema, :response},
           endpoint(id: "discount_rate"),
           section(id: "public")
         ] =
           path
       ) do
    was_empty =
      case schema do
        %{properties: properties} -> Enum.empty?(properties)
        _ -> true
      end

    schema_new =
      do_parse_block_schema(
        schema,
        block_data,
        block_parse_settings,
        if(was_empty, do: [[] | path], else: path)
      )

    if was_empty do
      %{type: :array, items: schema_new}
    else
      schema_new
    end
  end

  defp parse_block_schema(schema, block_data, block_parse_settings, path),
    do: do_parse_block_schema(schema, block_data, block_parse_settings, path)

  defp do_parse_block_schema(
         schema,
         block(node: node) = block_data,
         block_parse_settings(
           table_standalone_code_block_class: table_standalone_code_block_class
         ) = block_parse_settings,
         path
       ) do
    {properties, required} =
      parse_block_properties(schema, block_data, block_parse_settings, path)

    properties_new =
      with true <- is_binary(table_standalone_code_block_class),
           [code] <-
             Floki.find(node, "div.#{table_standalone_code_block_class} code.language-json") do
        %{properties: properties_new} =
          code
          |> extract_code()
          |> Jason.decode!()
          |> patch_schema_examples(%{type: :object, properties: properties}, path)

        properties_new
      else
        _ -> properties
      end

    {properties_new, required_new} =
      case block_data do
        block(update_operation: :new, update_name: name, description: description)
        when is_binary(name) ->
          schema = %{type: :object, properties: properties_new, description: description}

          schema_new =
            if Enum.empty?(required) do
              schema
            else
              Map.put(schema, :required, required)
            end

          {OrderedObject.new([{name, schema_new}]), []}

        _ ->
          {properties_new, required}
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

  defp parse_block_properties(
         %{type: :object} = schema,
         block(update_operation: :patch, update_type: :object, update_name: nil) = block_data,
         block_parse_settings,
         [{:schema, :response}, endpoint(id: "card_payment"), section(id: "internet_acquiring")] =
           path
       ) do
    {properties, required} =
      do_parse_block_properties(
        schema,
        block_data,
        block_parse_settings,
        path
      )

    properties_new =
      schema
      |> case do
        %{properties: properties} -> Enum.empty?(properties)
        _ -> true
      end
      |> if do
        ensure_block_properties(properties, [
          {"dcc_allowed",
           %{
             type: :array,
             description: "Data of alternative amount for payment with DCC",
             items: %{
               type: :object,
               properties:
                 OrderedObject.new([
                   {"amount",
                    %{
                      type: :number,
                      description: "Amount of payment in alternative currency"
                    }},
                   {"commission",
                    %{
                      type: :number,
                      description: "Commission on payment in alternative currency"
                    }},
                   {"currency",
                    %{
                      type: :string,
                      description: "Alternative currency"
                    }},
                   {"rate",
                    %{
                      type: :number,
                      description: "Conversion rate"
                    }}
                 ])
             }
           }}
        ])
      else
        properties
      end

    {properties_new, required}
  end

  defp parse_block_properties(
         %{type: :object} = schema,
         block(update_operation: :patch, update_type: :object, update_name: nil) = block_data,
         block_parse_settings,
         [{:schema, :response}, endpoint(id: "obtain"), section(id: "tokens")] =
           path
       ) do
    {properties, required} =
      do_parse_block_properties(
        schema,
        block_data,
        block_parse_settings,
        path
      )

    properties_new =
      schema
      |> case do
        %{properties: properties} -> Enum.empty?(properties)
        _ -> true
      end
      |> if do
        ensure_block_properties(properties, [
          {"result",
           %{
             type: :string,
             description: "The result of a request",
             enum: ["ok", "error"]
           }}
        ])
      else
        properties
      end

    {properties_new, required}
  end

  defp parse_block_properties(
         %{type: :object} = schema,
         block(update_operation: :patch, update_type: :object, update_name: nil) =
           block_data,
         block_parse_settings,
         [{:schema, :request}, endpoint(id: "MPI"), section(id: "confirmation")] =
           path
       ) do
    {properties, required} =
      do_parse_block_properties(
        schema,
        block_data,
        block_parse_settings,
        path
      )

    properties_new =
      schema
      |> case do
        %{properties: properties} -> Enum.empty?(properties)
        _ -> true
      end
      |> if do
        ensure_block_properties(properties, [
          {"phone", %{type: :string, description: "Payer's phone number"}}
        ])
      else
        properties
      end

    {properties_new, required}
  end

  defp parse_block_properties(
         schema,
         block(update_operation: :patch, update_type: :object, update_name: nil) = block_data,
         block_parse_settings,
         [{:schema, :response}, endpoint(id: "MPI"), section(id: "confirmation")] =
           path
       ) do
    {properties, required} =
      do_parse_block_properties(
        schema,
        block_data,
        block_parse_settings,
        path
      )

    properties_new =
      schema
      |> case do
        %{properties: properties} -> Enum.empty?(properties)
        _ -> true
      end
      |> if do
        ensure_block_properties(properties, [
          {"result",
           %{
             type: :string,
             description: "The result of a request",
             enum: ["ok", "error"]
           }},
          {"cres", %{type: :string, description: "CRes", nullable: true}}
        ])
      else
        properties
      end

    {properties_new, required}
  end

  defp parse_block_properties(
         schema,
         block(update_operation: :patch, update_type: :object, update_name: nil) = block_data,
         block_parse_settings,
         [{:schema, :response}, endpoint(id: "cardverification"), section(id: "confirmation")] =
           path
       ) do
    {properties, required} =
      do_parse_block_properties(
        schema,
        block_data,
        block_parse_settings,
        path
      )

    properties_new =
      schema
      |> case do
        %{properties: properties} -> Enum.empty?(properties)
        _ -> true
      end
      |> if do
        ensure_block_properties(properties, [
          {"rrn_debit",
           %{
             type: :string,
             description:
               "Unique transaction ID in authorization and settlement system of issuer bank `Retrieval Reference number`"
           }}
        ])
      else
        properties
      end

    {properties_new, required}
  end

  defp parse_block_properties(
         schema,
         block(update_operation: :patch, update_type: :object, update_name: nil) = block_data,
         block_parse_settings,
         [
           {:schema, :request},
           endpoint(id: "register"),
           section(id: "shop_create"),
           section(id: "partnership")
         ] =
           path
       ) do
    {properties, required} =
      do_parse_block_properties(
        schema,
        block_data,
        block_parse_settings,
        path
      )

    properties_new =
      schema
      |> case do
        %{properties: properties} -> Enum.empty?(properties)
        _ -> true
      end
      |> if do
        ensure_block_properties(properties, [
          {"description",
           %{
             type: :string,
             description: "Store description"
           }}
        ])
      else
        properties
      end

    {properties_new, required}
  end

  defp parse_block_properties(
         schema,
         block(update_operation: :patch, update_type: :object, update_name: nil) = block_data,
         block_parse_settings,
         [{:schema, :response}, endpoint(id: "payment_archive"), section(id: "information")] =
           path
       ) do
    was_empty =
      case schema do
        %{properties: properties} -> Enum.empty?(properties)
        _ -> true
      end

    {properties, required} =
      do_parse_block_properties(
        schema,
        block_data,
        block_parse_settings,
        if(was_empty, do: [[], "data" | path], else: path)
      )

    if was_empty do
      data_items = %{type: :object, properties: properties}

      data_items_new =
        if Enum.empty?(required) do
          data_items
        else
          Map.put(data_items, :required, required)
        end

      properties_new =
        OrderedObject.new([
          {"result",
           %{
             type: :string,
             description: "The result of a request",
             enum: ["ok", "error", "success"]
           }},
          {"data", %{type: :array, items: data_items_new}}
        ])

      {properties_new, []}
    else
      {properties, required}
    end
  end

  defp parse_block_properties(
         schema,
         block(update_operation: :patch, update_type: :object, update_name: nil) = block_data,
         block_parse_settings,
         [
           {:schema, :response},
           endpoint(id: endpoint_id),
           section(id: "register"),
           section(id: "information")
         ] =
           path
       )
       when endpoint_id in ~w(compensation_per_day compensation_per_transaction) do
    was_empty =
      case schema do
        %{properties: properties} -> Enum.empty?(properties)
        _ -> true
      end

    {properties, required} =
      do_parse_block_properties(
        schema,
        block_data,
        block_parse_settings,
        if(was_empty, do: [[], "data" | path], else: path)
      )

    if was_empty do
      data_items = %{type: :object, properties: properties}

      data_items_new =
        if Enum.empty?(required) do
          data_items
        else
          Map.put(data_items, :required, required)
        end

      properties_new =
        OrderedObject.new([
          {"result",
           %{
             type: :string,
             description: "The result of a request",
             enum: ["ok", "error", "success"]
           }},
          {"data", %{type: :array, items: data_items_new}}
        ])

      {properties_new, []}
    else
      {properties, required}
    end
  end

  defp parse_block_properties(
         schema,
         block(update_operation: :patch, update_type: :object, update_name: nil) = block_data,
         block_parse_settings,
         [{:schema, :response}, endpoint(id: "info_user"), section(id: "partnership")] =
           path
       ) do
    was_empty =
      case schema do
        %{properties: properties} -> Enum.empty?(properties)
        _ -> true
      end

    {properties, required} =
      do_parse_block_properties(
        schema,
        block_data,
        block_parse_settings,
        if(was_empty, do: [[], "data" | path], else: path)
      )

    if was_empty do
      {result_property, properties_new} = pop_in(properties, ["result"])
      data_items = %{type: :object, properties: properties_new}

      data_items_new =
        if Enum.empty?(required) do
          data_items
        else
          Map.put(data_items, :required, required -- ["result"])
        end

      properties_new =
        OrderedObject.new([
          {"result", result_property},
          {"data", %{type: :array, items: data_items_new}}
        ])

      {properties_new, []}
    else
      {properties, required}
    end
  end

  defp parse_block_properties(schema, block_data, block_parse_settings, path),
    do: do_parse_block_properties(schema, block_data, block_parse_settings, path)

  defp do_parse_block_properties(
         _schema,
         block(node: node) = _block_data,
         block_parse_settings(table_classes: table_classes) = block_parse_settings,
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

    column_headers =
      table
      |> Floki.find(
        "table.MuiTable-root thead.MuiTableHead-root tr.MuiTableRow-root.MuiTableRow-head th.MuiTableCell-root.MuiTableCell-head"
      )
      |> Enum.map(fn node ->
        node
        |> parse_node_text(block_parse_settings)
        |> String.downcase()
      end)

    {properties, required} =
      table
      |> Floki.find("table.MuiTable-root tbody.MuiTableBody-root tr.MuiTableRow-root")
      |> Enum.map(fn property ->
        {name, required, type, description, example_nodes} =
          property
          |> Floki.find("td.MuiTableCell-root.MuiTableCell-body")
          |> Enum.zip(column_headers)
          |> Enum.reduce({nil, false, :string, "", []}, fn
            {cell, "parameter"}, {_name, required, type, description, example_nodes} ->
              name = parse_property_name(cell, block_parse_settings)
              {name, required, type, description, example_nodes}

            {cell, "required"}, {name, _required, type, description, example_nodes} ->
              required = parse_property_required(cell, block_parse_settings)
              {name, required, type, description, example_nodes}

            {cell, "type"}, {name, required, _type, description, example_nodes} ->
              type = parse_property_type(cell, block_parse_settings)
              {name, required, type, description, example_nodes}

            {cell, "description"}, {name, required, type, _description, example_nodes} ->
              description =
                cell
                |> parse_node_text(block_parse_settings)
                |> String.replace(~r/\s*\.$/, "")

              {name, required, type, description, example_nodes}

            {cell, ""}, {name, required, type, description, example_nodes} ->
              {name, required, type, description, [cell | example_nodes]}
          end)

        path_new = [name | path]

        property_schema =
          %{type: type, description: description}
          |> initialize_property_processing(path_new)
          |> parse_property_format(path_new)
          |> parse_property_maximum_length(path_new)
          |> parse_property_enum(path_new)
          |> parse_property_default(path_new)
          |> parse_property_examples(path_new)
          |> parse_property_separate_example(example_nodes, path_new)

        {{name, property_schema}, if(required, do: [name], else: [])}
      end)
      |> Enum.unzip()

    properties_new = OrderedObject.new(properties)
    required_new = List.flatten(required)
    {properties_new, required_new}
  end

  defp ensure_block_properties(existing_properties, new_properties) do
    Enum.reduce(
      new_properties,
      existing_properties,
      fn {key, property}, properties ->
        case get_in(properties, [key]) do
          nil ->
            update_in(
              properties,
              [Access.key!(:values)],
              &List.keystore(&1, key, 0, {key, property})
            )

          _property ->
            properties
        end
      end
    )
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
    # TODO: Try to use existing property descriptions
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

  defp initialize_property_processing(
         %{type: :object, description: description} = property,
         [
           "dcc_allowed",
           {:schema, :response},
           endpoint(id: "dcc"),
           section(id: "internet_acquiring")
         ] = path
       ) do
    {description_new, key_descriptions} =
      ~r/^\s*`([^`]+)`\s*-\s*(.+)$/m
      |> Regex.scan(description)
      |> Enum.reduce({description, %{}}, fn [full_match, key, key_description],
                                            {description, key_descriptions} ->
        description_new =
          String.replace(description, full_match, "")

        key_descriptions_new = Map.put(key_descriptions, key, key_description)
        {description_new, key_descriptions_new}
      end)

    description_new =
      description_new
      |> String.trim()
      |> String.trim_trailing(":")

    property
    |> Map.merge(%{
      type: :array,
      description: description_new,
      items: %{
        type: :object,
        properties:
          OrderedObject.new([
            {
              "amount",
              %{
                type: :number,
                description: key_descriptions["amount"]
              }
            },
            {"commission",
             %{
               type: :number,
               description: key_descriptions["commission"]
             }},
            {"currency",
             %{
               type: :string,
               description: key_descriptions["currency"]
             }},
            {"rate",
             %{
               type: :number,
               description: key_descriptions["rate"]
             }}
          ])
      }
    })
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
         [integer_property, [], "data" | [{:schema, _schema_type} | _] = rest_path] = _path
       )
       when integer_property in ~w(version mpi_eci) do
    initialize_property_processing(property, [integer_property | rest_path])
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
         %{type: :array} = property,
         ["delivery_emails", "rro_info", {:schema, :request} | _] = path
       )
       when not is_map_key(property, :items) do
    property
    |> Map.put(:items, %{type: :string, format: :email})
    |> initialize_property_processing(path)
  end

  defp initialize_property_processing(
         %{type: :number} = property,
         [
           "register_token",
           {:schema, :response},
           endpoint(id: "compensation_report_p2p"),
           section(id: "register"),
           section(id: "information")
         ] = path
       ) do
    property
    |> Map.put(:type, :string)
    |> initialize_property_processing(path)
  end

  defp initialize_property_processing(
         %{type: :string} = property,
         [
           timestamp_property,
           {:schema, :request},
           endpoint(id: "payment_archive"),
           section(id: "information")
         ] = path
       )
       when timestamp_property in ~w(date_from date_to) do
    property
    |> Map.put(:type, :integer)
    |> initialize_property_processing(path)
  end

  defp initialize_property_processing(
         %{type: :string} = property,
         [
           timestamp_property,
           {:schema, :request},
           endpoint(id: "callback")
         ] = path
       )
       when timestamp_property in ~w(completion_date create_date end_date refund_date_last) do
    property
    |> Map.put(:type, :integer)
    |> initialize_property_processing(path)
  end

  defp initialize_property_processing(
         %{type: :string} = property,
         [
           "completion_date",
           {:schema, :response},
           endpoint(id: "complete"),
           section(id: "two_step"),
           section(id: "internet_acquiring")
         ] = path
       ) do
    property
    |> Map.put(:type, :integer)
    |> initialize_property_processing(path)
  end

  defp initialize_property_processing(
         %{type: :string} = property,
         [
           date_property,
           {:schema, :response},
           endpoint(id: "register"),
           section(id: "shop_create"),
           section(id: "partnership")
         ] = _path
       )
       when date_property in ~w(create_date update_date) do
    property
  end

  defp initialize_property_processing(
         %{type: :string} = property,
         [
           date_property,
           [],
           "data",
           {:schema, :response},
           endpoint(id: endpoint_id),
           section(id: "register"),
           section(id: "information")
         ] = _path
       )
       when endpoint_id in ~w(compensation_per_day compensation_per_transaction) and
              date_property in ~w(create_date end_date) do
    property
  end

  defp initialize_property_processing(
         %{type: :string} = property,
         [
           date_property,
           {:schema, :response},
           endpoint(id: "register"),
           section(id: "shop_create"),
           section(id: "partnership")
         ] = _path
       )
       when date_property in ~w(create_date update_date) do
    property
  end

  defp initialize_property_processing(
         property,
         [timestamp_property, [], "data" | [{:schema, :response} | _] = rest_path] = _path
       )
       when timestamp_property in ~w(create_date end_date) do
    initialize_property_processing(property, [timestamp_property | rest_path])
  end

  defp initialize_property_processing(
         %{type: :string} = property,
         [timestamp_property, {:schema, :response} | _] = path
       )
       when timestamp_property in ~w(create_date end_date) do
    property
    |> Map.put(:type, :integer)
    |> initialize_property_processing(path)
  end

  defp initialize_property_processing(
         %{type: :string} = property,
         [
           "update_date",
           [],
           "data",
           {:schema, :response},
           endpoint(id: "info_user"),
           section(id: "partnership")
         ] = path
       ) do
    property
    |> Map.put(:type, :integer)
    |> initialize_property_processing(path)
  end

  defp initialize_property_processing(
         %{type: :string} = property,
         [
           "update_date",
           {:schema, :response},
           endpoint(id: "info_merchant"),
           section(id: "partnership")
         ] = path
       ) do
    property
    |> Map.put(:type, :integer)
    |> initialize_property_processing(path)
  end

  defp initialize_property_processing(
         %{type: :string} = property,
         [
           "rate_value",
           [],
           {:schema, :response},
           endpoint(id: "discount_rate"),
           section(id: "public")
         ] = path
       ) do
    property
    |> Map.put(:type, :number)
    |> initialize_property_processing(path)
  end

  defp initialize_property_processing(
         %{type: :string} = property,
         [
           rate_property,
           [],
           "exchangeRate",
           {:schema, :response},
           endpoint(id: "archive"),
           section(id: "public")
         ] = path
       )
       when rate_property in ~w(saleRateNB/purchaseRateNB saleRateNB purchaseRateNB saleRate purchaseRate) do
    property
    |> Map.put(:type, :number)
    |> initialize_property_processing(path)
  end

  defp initialize_property_processing(
         %{type: :string} = property,
         [
           "goods",
           {:schema, :request},
           endpoint(id: "issue"),
           section(id: "invoice"),
           section(id: "internet_acquiring")
         ] = path
       ) do
    property
    |> Map.merge(%{
      type: :array,
      items: %{
        type: :object,
        required: ["amount", "count", "unit", "price"],
        properties:
          OrderedObject.new([
            {"amount", %{type: :number, description: "Quantity/volume"}},
            {"count", %{type: :integer, description: "Count"}},
            {"unit", %{type: :string, description: "Unit"}},
            {"name", %{type: :string, description: "Name"}}
          ])
      }
    })
    |> initialize_property_processing(path)
  end

  defp initialize_property_processing(property, _path), do: property

  defp search_code_blocks(
         {"div", _, _} = div,
         block_parse_settings(
           title_classes: title_classes,
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
      not (classes
           |> MapSet.intersection(title_classes)
           |> Enum.empty?()) ->
        title_new = parse_block_title_text(div, block_parse_settings)
        {title_new, subtitle, code_blocks}

      not (classes
           |> MapSet.intersection(subtitle_classes)
           |> Enum.empty?()) ->
        subtitle_new = parse_block_title_text(div, block_parse_settings)
        {title, subtitle_new, code_blocks}

      code_block_class && MapSet.member?(classes, code_block_class) ->
        {title, subtitle}
        |> parse_block_title(block_parse_settings)
        |> downcase_block_title()
        |> parse_standalone_example(div, block_parse_settings, path)
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
        |> patch_schema_examples(%{schema | description: description_new}, path)

      [
        "goods",
        {:schema, :request},
        endpoint(id: "issue"),
        section(id: "invoice"),
        section(id: "internet_acquiring")
      ] ->
        code
        |> extract_code()
        |> Jason.decode!()
        |> patch_schema_examples(schema, path)
    end
  end

  defp parse_property_separate_example(schema, [{_, _, _} = node], path) do
    parse_property_separate_example(schema, Floki.find(node, "code.language-json"), path)
  end

  defp patch_schema_examples(example, %{type: :object, properties: _properties} = schema, path)
       when is_map(example) do
    Enum.reduce(
      example,
      schema,
      fn {key, value}, schema -> patch_object_schema_examples(value, schema, [key | path]) end
    )
  end

  defp patch_schema_examples(example, %{type: :object} = schema, path) do
    IO.inspect(example, label: "Invalid object example (#{loggable_schema_path(path)})")
    schema
  end

  defp patch_schema_examples(example, %{type: :array, items: items} = schema, path)
       when is_list(example) do
    items_new =
      Enum.reduce(
        example,
        items,
        &patch_schema_examples(&1, &2, [[] | path])
      )

    %{schema | items: items_new}
  end

  defp patch_schema_examples(example, %{type: :array} = schema, path) do
    IO.inspect(example, label: "Invalid array example (#{loggable_schema_path(path)})")
    schema
  end

  defp patch_schema_examples(example, %{oneOf: schemas} = schema, path) do
    schemas_new = List.update_at(schemas, -1, &patch_schema_examples(example, &1, path))
    %{schema | oneOf: schemas_new}
  end

  defp patch_schema_examples(nil, schema, _path), do: schema
  defp patch_schema_examples("null", schema, _path), do: schema

  defp patch_schema_examples(example, schema, _path) do
    example_new = parse_schema_value(example, schema)
    Map.update(schema, :examples, [example_new], &Enum.uniq(&1 ++ [example_new]))
  end

  defp loggable_schema_path(path) do
    path
    |> Enum.map(fn
      key when is_binary(key) -> key
      [] -> []
      {:schema, _} = schema -> schema
      section(id: id) -> id
      endpoint(id: id) -> id
    end)
    |> inspect()
  end

  defp patch_object_schema_examples(
         _example,
         %{type: :object} = schema,
         [
           key,
           {:schema, :request},
           endpoint(id: "create"),
           section(id: "shop_create"),
           section(id: "partnership")
         ] = _path
       )
       when key in @partnership_card_fields do
    schema
  end

  defp patch_object_schema_examples(
         _example,
         %{type: :object} = schema,
         [
           key,
           {:schema, :request},
           section(id: "shop_edit"),
           section(id: "partnership")
         ] = _path
       )
       when key in @partnership_card_fields do
    schema
  end

  defp patch_object_schema_examples(
         example,
         %{type: :object} = schema,
         [key | [{:schema, :request}, _, section(id: "internet_acquiring")] = rest_path] = _path
       )
       when key in @internet_acquiring_regular_payment_fields do
    patch_schema_examples(%{"regular_payment" => %{key => example}}, schema, rest_path)
  end

  defp patch_object_schema_examples(
         example,
         %{type: :object} = schema,
         [key, {:schema, :request}, _, section, section(id: "internet_acquiring")] = _path
       )
       when key in @internet_acquiring_regular_payment_fields do
    patch_object_schema_examples(
      example,
      schema,
      [key, {:schema, :request}, section, section(id: "internet_acquiring")]
    )
  end

  defp patch_object_schema_examples(
         example,
         %{type: :object, properties: properties} = schema,
         [key | _] = path
       ) do
    properties_new =
      case get_in(properties, [key]) do
        nil ->
          IO.inspect(example, label: "Unused example (#{loggable_schema_path(path)})")
          properties

        current ->
          put_in(properties, [key], patch_schema_examples(example, current, path))
      end

    %{schema | properties: properties_new}
  end

  defp parse_property_format(
         %{type: :string} = property,
         [boolean_property, {:schema, _schema_type} | _] = path
       )
       when boolean_property in ~w(verifycode) and
              not is_map_key(property, :format) do
    property
    |> Map.put(:format, @boolean_yesno_format)
    |> parse_property_format(path)
  end

  defp parse_property_format(
         %{type: :string} = property,
         [boolean_property, {:schema, _schema_type} | _] = path
       )
       when boolean_property in ~w(subscribe prepare sandbox) and
              not is_map_key(property, :format) do
    property
    |> Map.put(:format, @boolean_integer_format)
    |> parse_property_format(path)
  end

  defp parse_property_format(
         %{type: :string} = property,
         ["recurringbytoken", {:schema, :request} | _] = path
       )
       when not is_map_key(property, :format) do
    property
    |> Map.put(:format, @boolean_integer_format)
    |> parse_property_format(path)
  end

  defp parse_property_format(
         property,
         ["recurringbytoken", "one_click_payment" | [{:schema, :request} | _] = rest_path] = _path
       ) do
    parse_property_format(property, ["recurringbytoken" | rest_path])
  end

  defp parse_property_format(
         %{type: :string} = property,
         [url_property, {:schema, _schema_type} | _] = path
       )
       when url_property in ~w(result_url server_url product_url) and
              not is_map_key(property, :format) do
    property
    |> Map.put(:format, :uri)
    |> parse_property_format(path)
  end

  defp parse_property_format(
         %{type: :string} = property,
         [datetime_property, {:schema, _schema_type} | _] = path
       )
       when datetime_property in ~w(expired_date) and
              not is_map_key(property, :format) do
    property
    |> Map.put(:format, @date_time_liqpay_format)
    |> parse_property_format(path)
  end

  defp parse_property_format(
         %{type: :string} = property,
         ["subscribe_date_start", "regular_payment", {:schema, :request} | _] = path
       )
       when not is_map_key(property, :format) do
    property
    |> Map.put(:format, @date_time_liqpay_format)
    |> parse_property_format(path)
  end

  defp parse_property_format(
         %{type: :string} = property,
         [
           "rate_date",
           [],
           {:schema, :response},
           endpoint(id: "discount_rate"),
           section(id: "public")
         ] =
           path
       )
       when not is_map_key(property, :format) do
    property
    |> Map.put(:format, @date_liqpay_format)
    |> parse_property_format(path)
  end

  defp parse_property_format(
         %{type: :string} = property,
         [
           "birth_date",
           "law_cto_info",
           "aggregator",
           {:schema, :request},
           endpoint(),
           section(id: "shop_create"),
           section(id: "partnership")
         ] = path
       )
       when not is_map_key(property, :format) do
    property
    |> Map.put(:format, :date)
    |> parse_property_format(path)
  end

  defp parse_property_format(
         property,
         [
           "birth_date",
           [],
           _
           | [
               "aggregator",
               {:schema, :request},
               endpoint(),
               section(id: "shop_create"),
               section(id: "partnership")
             ] = rest_path
         ] = _path
       ) do
    parse_property_format(property, ["birth_date", "law_cto_info" | rest_path])
  end

  defp parse_property_format(
         %{type: :string} = property,
         [
           date_property,
           {:schema, :response},
           endpoint(id: "register"),
           section(id: "shop_create"),
           section(id: "partnership")
         ] = path
       )
       when date_property in ~w(create_date update_date) and not is_map_key(property, :format) do
    property
    |> Map.put(:format, :date)
    |> parse_property_format(path)
  end

  defp parse_property_format(
         %{type: :string} = property,
         [
           "date",
           {:schema, :request},
           endpoint(id: endpoint_id),
           section(id: "register"),
           section(id: "information")
         ] = path
       )
       when endpoint_id in ~w(compensation_per_day compensation_per_transaction compensation_report compensation_report_p2p) and
              not is_map_key(property, :format) do
    property
    |> Map.put(:format, :date)
    |> parse_property_format(path)
  end

  defp parse_property_format(
         %{type: :string} = property,
         [
           date_property,
           [],
           "data",
           {:schema, :response},
           endpoint(id: endpoint_id),
           section(id: "register"),
           section(id: "information")
         ] = path
       )
       when endpoint_id in ~w(compensation_per_day compensation_per_transaction) and
              date_property in ~w(create_date end_date) and not is_map_key(property, :format) do
    property
    |> Map.put(:format, @date_time_liqpay_format)
    |> parse_property_format(path)
  end

  defp parse_property_format(
         %{type: :integer} = property,
         [
           date_property,
           [],
           "data",
           {:schema, :response},
           endpoint(id: "payment_archive"),
           section(id: "information")
         ] = path
       )
       when date_property in ~w(create_date end_date) and not is_map_key(property, :format) do
    property
    |> Map.put(:format, @timestamp_ms_format)
    |> parse_property_format(path)
  end

  defp parse_property_format(
         property,
         [timestamp_property, [], "data" | [{:schema, :response} | _] = rest_path] = _path
       )
       when timestamp_property in ~w(create_date end_date) do
    parse_property_format(property, [timestamp_property | rest_path])
  end

  defp parse_property_format(
         %{type: :integer} = property,
         [timestamp_property, {:schema, :response} | _] = path
       )
       when timestamp_property in ~w(create_date end_date) and
              not is_map_key(property, :format) do
    property
    |> Map.put(:format, @timestamp_ms_format)
    |> parse_property_format(path)
  end

  defp parse_property_format(
         %{type: :integer} = property,
         [
           date_property,
           {:schema, :request},
           endpoint(id: "payment_archive"),
           section(id: "information")
         ] = path
       )
       when date_property in ~w(date_from date_to) and not is_map_key(property, :format) do
    property
    |> Map.put(:format, @timestamp_ms_format)
    |> parse_property_format(path)
  end

  defp parse_property_format(
         %{type: :integer} = property,
         [
           timestamp_property,
           {:schema, :request},
           endpoint(id: "callback")
         ] = path
       )
       when timestamp_property in ~w(completion_date create_date end_date refund_date_last) and
              not is_map_key(property, :format) do
    property
    |> Map.put(:format, @timestamp_ms_format)
    |> parse_property_format(path)
  end

  defp parse_property_format(
         %{type: :string} = property,
         [
           "tokenExpDate",
           "card_token_info",
           {:schema, :response},
           endpoint(id: endpoint_id),
           section(id: "tokens")
         ] = path
       )
       when endpoint_id in ~w(obtain change_status) and not is_map_key(property, :format) do
    property
    |> Map.put(:format, @month_year_liqpay_format)
    |> parse_property_format(path)
  end

  defp parse_property_format(
         %{type: :integer} = property,
         [
           "completion_date",
           {:schema, :response},
           endpoint(id: "complete"),
           section(id: "two_step"),
           section(id: "internet_acquiring")
         ] = path
       )
       when not is_map_key(property, :format) do
    property
    |> Map.put(:format, @timestamp_ms_format)
    |> parse_property_format(path)
  end

  defp parse_property_format(
         %{type: :integer} = property,
         [
           "update_date",
           [],
           "data",
           {:schema, :response},
           endpoint(id: "info_user"),
           section(id: "partnership")
         ] = path
       )
       when not is_map_key(property, :format) do
    property
    |> Map.put(:format, @timestamp_ms_format)
    |> parse_property_format(path)
  end

  defp parse_property_format(
         %{type: :integer} = property,
         [
           "update_date",
           {:schema, :response},
           endpoint(id: "info_merchant"),
           section(id: "partnership")
         ] = path
       )
       when not is_map_key(property, :format) do
    property
    |> Map.put(:format, @timestamp_ms_format)
    |> parse_property_format(path)
  end

  defp parse_property_format(property, _path), do: property

  defp parse_property_maximum_length(%{description: description} = property, _path) do
    ~r/(?:\.\s+)?(?:The\s+m|M)ax(?:imum)?\s+length(?:\s+is)?\s+(\*\*)?(\d+)\1?\s+(?:character|symbol)s?/
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

  defp parse_property_enum(%{description: description} = property, path)
       when not is_map_key(property, :enum) do
    ~r/(\.\s+)?(?:Possible|Valid)\s+values?\s*:?\n?([^\.\n]+)(?=\.|$)/i
    |> Regex.scan(description)
    |> case do
      [[full_match, prefix, values_match]] ->
        {enum_options, has_descriptions} =
          ~r/\s*`([^`]+?)`(?:\s+[\-\–]\s+([^\.\n`]+))?(?:[\n,\.]|$)/u
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

        enum =
          enum_options
          |> Enum.map(fn {key, _} ->
            parse_schema_value(key, property)
          end)
          |> Enum.uniq()

        property
        |> Map.merge(%{
          enum: enum,
          description: description_new
        })
        |> parse_property_enum_specific(path)

      [] ->
        ~r/^((?:\s*`[^`]+?`,?)+)$/
        |> Regex.match?(description)
        |> if do
          property
          |> parse_property_enum_list(path)
          |> Map.delete(:description)
          |> parse_property_enum_specific(path)
        else
          ~r/^\s*`([^`]+)`\s*-\s*(.+)$/m
          |> Regex.scan(description)
          |> case do
            [] ->
              parse_property_enum_specific(property, path)

            matches ->
              Enum.reduce(matches, property, fn [full_match, key, key_description],
                                                %{description: description} = property_new ->
                description_new =
                  String.replace(description, full_match, "* `#{key}` - #{key_description}")

                key_new = parse_schema_value(key, property_new)

                property_new
                |> Map.update(:enum, [key_new], &Enum.uniq(&1 ++ [key_new]))
                |> Map.put(:description, description_new)
              end)
              |> parse_property_enum_specific(path)
          end
        end
    end
  end

  defp parse_property_enum(property, path), do: parse_property_enum_specific(property, path)

  defp parse_property_enum_list(%{description: description} = property, _path) do
    ~r/`([^`]+?)`/
    |> Regex.scan(description, capture: :all_but_first)
    |> Enum.flat_map(fn [str] -> String.split(str, ",") end)
    |> Enum.reduce(property, fn enum, property ->
      enum_new = enum |> String.trim() |> parse_schema_value(property)
      Map.update(property, :enum, [enum_new], &Enum.uniq(&1 ++ [enum_new]))
    end)
  end

  defp parse_property_enum_specific(
         %{type: :string, description: description} = property,
         ["language", {:schema, :request} | _] = path
       )
       when not is_map_key(property, :enum) do
    ~r/(?:\.\s+)?Customer's\s+language(\s+[^\.\n]+)(?=\.|$)/i
    |> Regex.scan(description, capture: :all_but_first)
    |> case do
      [[values_match]] ->
        description_new = String.replace(description, values_match, "")

        %{property | description: values_match}
        |> parse_property_enum_list(path)
        |> Map.put(:description, description_new)

      [] ->
        ~r/(?:\.\s+)?The\s+meaning\s+of(\s+[^\.\n]+)(?=\.|$)/i
        |> Regex.scan(description)
        |> case do
          [[full_match, values_match]] ->
            description_new = String.replace(description, full_match, "")

            %{property | description: values_match}
            |> parse_property_enum(path)
            |> Map.put(:description, description_new)

          [] ->
            property
        end
    end
    |> Map.put_new(:enum, ["uk", "en"])
    |> parse_property_enum_specific(path)
  end

  defp parse_property_enum_specific(
         %{description: description} = property,
         [
           "resp_format",
           {:schema, :request},
           endpoint(id: "payment_archive"),
           section(id: "information")
         ] =
           path
       )
       when not is_map_key(property, :enum) do
    ~r/(?:\.\s+)?Possible\s+report\s+format\s+\n?([^\.\n]+)/ui
    |> Regex.scan(description)
    |> case do
      [[full_match, values_match]] ->
        description_new =
          description
          |> String.replace(full_match, ". Report format")
          |> String.replace(~r/^\s*\.\s*/, "")

        %{property | description: values_match}
        |> parse_property_enum_list(path)
        |> Map.put(:description, description_new)

      [] ->
        property
    end
    |> Map.put_new(:enum, ["json", "csv", "xml"])
    |> parse_property_enum_specific(path)
  end

  defp parse_property_enum_specific(
         property,
         [
           "resp_format",
           {:schema, :request},
           endpoint(id: "compensation_per_day"),
           section(id: "register") | [section(id: "information")] = rest_path
         ] =
           _path
       ) do
    parse_property_enum_specific(property, [
      "resp_format",
      {:schema, :request},
      endpoint(id: "payment_archive") | rest_path
    ])
  end

  defp parse_property_enum_specific(
         %{description: description} = property,
         [
           "resp_format",
           {:schema, :request},
           endpoint(id: "compensation_report_p2p"),
           section(id: "register"),
           section(id: "information")
         ] =
           path
       )
       when not is_map_key(property, :enum) do
    ~r/(?:\.\s+)?Possible\s+report\s+format\s*\:?\s+\n?([^\.\n\(]+)/ui
    |> Regex.scan(description)
    |> case do
      [[full_match, values_match]] ->
        description_new =
          description
          |> String.replace(full_match, ". Report format")
          |> String.replace(~r/^\s*\.\s*/, "")

        %{property | description: values_match}
        |> parse_property_enum_list(path)
        |> Map.put(:description, description_new)

      [] ->
        property
    end
    |> Map.put_new(:enum, ["csv"])
    |> parse_property_enum_specific(path)
  end

  defp parse_property_enum_specific(
         %{type: :string} = property,
         [boolean_property, {:schema, _schema_type} | _] = path
       )
       when boolean_property in ~w(verifycode) and
              not is_map_key(property, :enum) do
    property
    |> Map.put(:enum, ["Y"])
    |> parse_property_enum_specific(path)
  end

  defp parse_property_enum_specific(
         %{type: :string} = property,
         [boolean_property, {:schema, _schema_type} | _] = path
       )
       when boolean_property in ~w(subscribe prepare sandbox) and not is_map_key(property, :enum) do
    property
    |> Map.put(:enum, ["1"])
    |> parse_property_enum_specific(path)
  end

  defp parse_property_enum_specific(
         %{type: :string} = property,
         ["recurringbytoken", {:schema, :request} | _] = path
       )
       when not is_map_key(property, :enum) do
    property
    |> Map.put(:enum, ["1"])
    |> parse_property_enum_specific(path)
  end

  defp parse_property_enum_specific(
         property,
         ["recurringbytoken", "one_click_payment" | [{:schema, :request} | _] = rest_path] = _path
       ) do
    parse_property_enum_specific(property, ["recurringbytoken" | rest_path])
  end

  defp parse_property_enum_specific(
         %{description: description} = property,
         ["mpi_version", {:schema, :response}, endpoint(id: "MPI"), section(id: "confirmation")] =
           path
       )
       when not is_map_key(property, :enum) do
    ~r/(?:\.\s+)?Value\s+`"([^`]+?)"`(?=\.|$)/ui
    |> Regex.scan(description)
    |> case do
      [[full_match, enum]] ->
        description_new =
          description
          |> String.replace(full_match, "")
          |> String.replace(~r/^\s*\.\s*/, "")

        %{property | description: "`#{enum}`"}
        |> parse_property_enum_list(path)
        |> Map.put(:description, description_new)

      [] ->
        property
    end
    |> Map.put_new(:enum, ["2.0"])
    |> parse_property_enum_specific(path)
  end

  defp parse_property_enum_specific(property, _path), do: property

  defp parse_property_default(
         %{description: description} = property,
         ["version", {:schema, _schema_type} | _] = path
       )
       when not is_map_key(property, :default) do
    ~r/(?:\.\s+)?(?:Current|Present)\s+value\s*[\-\–]?\s*`(\d+)`(?=\.|$)/ui
    |> Regex.scan(description)
    |> case do
      [[full_match, default]] ->
        description_new = String.replace(description, full_match, "")
        default_new = parse_schema_value(default, property)
        Map.merge(property, %{default: default_new, description: description_new})

      [] ->
        property
    end
    |> Map.put_new(:default, 3)
    |> Map.put_new(:enum, [3])
    |> parse_property_default(path)
  end

  defp parse_property_default(
         property,
         ["version", [], "data" | [{:schema, _schema_type} | _] = rest_path] = _path
       ) do
    parse_property_default(property, ["version" | rest_path])
  end

  defp parse_property_default(
         %{description: description} = property,
         ["action_payment", {:schema, :request}, endpoint(id: "MPI"), section(id: "confirmation")] =
           path
       )
       when not is_map_key(property, :default) do
    ~r/(?:\.\s+)?Default\s+value\s+is\s+(\w+)\s*[\-\–]\s*`\1`(?=\.|$)/ui
    |> Regex.scan(description)
    |> case do
      [[full_match, default]] ->
        description_new = String.replace(description, full_match, "")
        default_new = parse_schema_value(default, property)
        Map.merge(property, %{default: default_new, description: description_new})

      [] ->
        property
    end
    |> Map.put_new(:default, 3)
    |> parse_property_default(path)
  end

  defp parse_property_default(
         %{description: description} = property,
         [
           "browserJavascriptEnabled",
           "threeDSInfo",
           {:schema, :request},
           endpoint(id: "MPI"),
           section(id: "confirmation")
         ] =
           path
       )
       when not is_map_key(property, :default) do
    ~r/(?:\.\s+)?Default\s+`(\w+)`(?=\.|$)/ui
    |> Regex.scan(description)
    |> case do
      [[full_match, default]] ->
        description_new = String.replace(description, full_match, "")
        default_new = parse_schema_value(default, property)
        Map.merge(property, %{default: default_new, description: description_new})

      [] ->
        property
    end
    |> Map.put_new(:default, true)
    |> parse_property_default(path)
  end

  defp parse_property_default(
         %{description: description} = property,
         [
           "browserJavaEnabled",
           "threeDSInfo",
           {:schema, :request},
           endpoint(id: "MPI"),
           section(id: "confirmation")
         ] =
           path
       )
       when not is_map_key(property, :default) do
    ~r/(?:\.\s+)?Default\s+`(\w+)`(?=\.|$)/ui
    |> Regex.scan(description)
    |> case do
      [[full_match, default]] ->
        description_new = String.replace(description, full_match, "")
        default_new = parse_schema_value(default, property)
        Map.merge(property, %{default: default_new, description: description_new})

      [] ->
        property
    end
    |> Map.put_new(:default, false)
    |> parse_property_default(path)
  end

  defp parse_property_default(
         %{type: :string} = property,
         ["language", {:schema, :request} | _] = path
       )
       when not is_map_key(property, :default) do
    property
    |> Map.put_new(:default, "uk")
    |> Map.replace_lazy(
      :description,
      &String.replace(
        &1,
        ~r/(?:\.\s+)?The\s+default\s+language\s+is\s+[[:upper:]]\w+(?=\.|$)/i,
        ""
      )
    )
    |> parse_property_default(path)
  end

  defp parse_property_default(
         %{description: description} = property,
         [
           "resp_format",
           {:schema, :request},
           endpoint(id: "payment_archive"),
           section(id: "information")
         ] =
           path
       )
       when not is_map_key(property, :default) do
    ~r/(?:\.\s+)?If\s+parameter\s+is\s+not\s+passed\s*,\s+will\s+be\s+passed\s+by\s+default\s+`(\w+)`(?=\.|$)/ui
    |> Regex.scan(description)
    |> case do
      [[full_match, default]] ->
        description_new = String.replace(description, full_match, "")
        default_new = parse_schema_value(default, property)
        Map.merge(property, %{default: default_new, description: description_new})

      [] ->
        property
    end
    |> Map.put_new(:default, "json")
    |> parse_property_default(path)
  end

  defp parse_property_default(
         property,
         [
           "resp_format",
           {:schema, :request},
           endpoint(id: "compensation_per_day"),
           section(id: "register") | [section(id: "information")] = rest_path
         ] =
           _path
       ) do
    parse_property_default(property, [
      "resp_format",
      {:schema, :request},
      endpoint(id: "payment_archive") | rest_path
    ])
  end

  defp parse_property_default(
         %{description: description} = property,
         [
           "resp_format",
           {:schema, :request},
           endpoint(id: "compensation_report_p2p"),
           section(id: "register"),
           section(id: "information")
         ] =
           path
       )
       when not is_map_key(property, :default) do
    ~r/\s*\(\s*default\s+"(\w+)"\s*\)(?=\.|$)/ui
    |> Regex.scan(description)
    |> case do
      [[full_match, default]] ->
        description_new = String.replace(description, full_match, "")
        default_new = parse_schema_value(default, property)
        Map.merge(property, %{default: default_new, description: description_new})

      [] ->
        property
    end
    |> Map.put_new(:default, "csv")
    |> parse_property_default(path)
  end

  defp parse_property_default(property, _path), do: property

  defp parse_property_examples(%{description: description} = property, path) do
    ~r/(?:\.\s+)?(?:For\s+example)[:,]?((?:\s*`[^`]+?`,?)+)\s*(?:\(([^\)]+)\))?(?=\.|$)/
    |> Regex.scan(description)
    |> case do
      [[full_match, examples_match]] ->
        property
        |> process_examples_match_in_description(full_match, examples_match, path)
        |> parse_property_examples_specific(path)

      [[full_match, examples_match, explanation]] ->
        description_new = String.replace(description, full_match, "#{full_match}. #{explanation}")

        %{property | description: description_new}
        |> process_examples_match_in_description(full_match, examples_match, path)
        |> parse_property_examples_specific(path)

      [] ->
        parse_property_examples_specific(property, path)
    end
  end

  defp parse_property_examples(property, path),
    do: parse_property_examples_specific(property, path)

  defp parse_property_examples_specific(
         %{description: description} = property,
         ["phone", {:schema, :request} | _] = path
       ) do
    ~r/(?:\.\s+)?For\s+example:?(.*?)(?=\.|$)/ui
    |> Regex.scan(description)
    |> case do
      [[full_match, examples]] ->
        description_new =
          description
          |> String.replace(full_match, "")
          |> String.replace(~r/^\s*\.\s*/, "")

        ~r/\s*(\+?\d+)(?:\s+\(\s*\w+\s+\+\))?/
        |> Regex.scan(examples, capture: :all_but_first)
        |> Enum.reduce(%{property | description: description_new}, fn [example], property ->
          patch_schema_examples(example, property, path)
        end)

      [] ->
        Enum.reduce(
          ["+380950000001", "380950000001"],
          property,
          &patch_schema_examples(&1, &2, path)
        )
    end
  end

  defp parse_property_examples_specific(
         %{description: description} = property,
         [
           "citizenship",
           "law_cto_info",
           "aggregator",
           {:schema, :request},
           endpoint(),
           section(id: "shop_create"),
           section(id: "partnership")
         ] = path
       ) do
    ~r/(?:\.\s+)?Example:?\s*(\w+)\s*(?=\.|$)/ui
    |> Regex.scan(description)
    |> case do
      [[full_match, example]] ->
        description_new =
          description
          |> String.replace(full_match, "")
          |> String.replace(~r/^\s*\.\s*/, "")

        patch_schema_examples(example, %{property | description: description_new}, path)

      [] ->
        patch_schema_examples("Ukraine", property, path)
    end
  end

  defp parse_property_examples_specific(
         property,
         [
           "citizenship",
           [],
           _
           | [
               "aggregator",
               {:schema, :request},
               endpoint(),
               section(id: "shop_create"),
               section(id: "partnership")
             ] = rest_path
         ] = _path
       ) do
    parse_property_examples_specific(property, ["citizenship", "law_cto_info" | rest_path])
  end

  defp parse_property_examples_specific(
         %{description: description} = property,
         [
           "browserLanguage",
           "threeDSInfo",
           {:schema, :request},
           endpoint(id: "MPI"),
           section(id: "confirmation")
         ] =
           path
       ) do
    ~r/(?:\.\s+)?For\s+example[:,]?\s*([\w\-]+)(?=\.|$)/ui
    |> Regex.scan(description)
    |> case do
      [[full_match, example]] ->
        description_new =
          description
          |> String.replace(full_match, "")
          |> String.replace(~r/^\s*\.\s*/, "")

        patch_schema_examples(example, %{property | description: description_new}, path)

      [] ->
        patch_schema_examples("en-US", property, path)
    end
  end

  defp parse_property_examples_specific(
         %{description: description} = property,
         [
           "browserTZ",
           "threeDSInfo",
           {:schema, :request},
           endpoint(id: "MPI"),
           section(id: "confirmation")
         ] =
           path
       ) do
    ~r/(?:\.\s+)?Example\s+of\s+[^\:]+\:(.*?)(?=\.|$)/uis
    |> Regex.scan(description)
    |> case do
      [[full_match, examples]] ->
        description_new =
          description
          |> String.replace(full_match, "")
          |> String.replace(~r/^\s*\.\s*/, "")

        ~r/[\-\-]\s*if\s+UTC\s+(?:is\s+)?[\-\–\+]\d+\s+hours\s*,\s*then\s+the\s+value\s+is\s+([\-\–]?\d+)\s*/iu
        |> Regex.scan(examples, capture: :all_but_first)
        |> Enum.reduce(%{property | description: description_new}, fn [example], property ->
          patch_schema_examples(example, property, path)
        end)

      [] ->
        Enum.reduce([300, -300], property, &patch_schema_examples(&1, &2, path))
    end
  end

  defp parse_property_examples_specific(
         %{description: description} = property,
         ["year", {:schema, :request}, endpoint(id: "discount_rate"), section(id: "public")] =
           path
       ) do
    ~r/,\s*for\s+example\s*[\-\–]\s*(\d+)(?=\.|$)/ui
    |> Regex.scan(description)
    |> case do
      [[full_match, example]] ->
        description_new =
          description
          |> String.replace(full_match, "")
          |> String.replace(~r/^\s*\.\s*/, "")

        patch_schema_examples(example, %{property | description: description_new}, path)

      [] ->
        patch_schema_examples("2014", property, path)
    end
  end

  defp parse_property_examples_specific(property, _path), do: property

  defp process_examples_match_in_description(
         %{description: description} = property,
         full_match,
         match,
         path
       ) do
    description_new =
      description
      |> String.replace(full_match, "")
      |> String.replace(~r/^\s*\.\s*/, "")

    ~r/`([^`]+?)`/
    |> Regex.scan(match, capture: :all_but_first)
    |> Enum.reduce(%{property | description: description_new}, fn [example], property ->
      example_new =
        ~r/^«(.+)»$/s
        |> Regex.scan(example, capture: :all_but_first)
        |> case do
          [[stripped_example]] -> stripped_example
          [] -> example
        end

      patch_schema_examples(example_new, property, path)
    end)
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

  defp append_schema_object_properties(schema, properties_new) do
    update_in(
      schema,
      [Access.key(:properties, OrderedObject.new([])), Access.key!(:values)],
      &(&1 ++ properties_new.values)
    )
  end

  # FIXME: No response description
  defp parse_standalone_example("response example", _div, _block_parse_settings, [
         endpoint(id: "decrypted_token"),
         section(id: "gpay"),
         section(id: "internet_acquiring")
       ]),
       do: :error

  defp parse_standalone_example("response example", div, block_parse_settings, path),
    do: parse_standalone_example(false, div, block_parse_settings, path)

  defp parse_standalone_example(
         "sample response for mastercard",
         div,
         block_parse_settings,
         [endpoint(id: "obtain"), section(id: "tokens")] = path
       ),
       do: parse_standalone_example(false, div, block_parse_settings, path)

  defp parse_standalone_example(
         "sample response for visa",
         div,
         block_parse_settings,
         [endpoint(id: "obtain"), section(id: "tokens")] = path
       ),
       do: parse_standalone_example(false, div, block_parse_settings, path)

  defp parse_standalone_example(
         {"example response", _},
         div,
         block_parse_settings,
         [endpoint(id: "MPI"), section(id: "confirmation")] = path
       ),
       do: parse_standalone_example(false, div, block_parse_settings, path)

  defp parse_standalone_example(
         "check of the callback of the signature",
         _div,
         _block_parse_settings,
         [section(id: "callback")]
       ),
       do: :error

  defp parse_standalone_example("example of using sdk", div, block_parse_settings, path),
    do: parse_standalone_example(true, div, block_parse_settings, path)

  defp parse_standalone_example(
         "connection",
         div,
         block_parse_settings,
         [section(id: "checkout"), section(id: "internet_acquiring")] = path
       ),
       do: parse_standalone_example(true, div, block_parse_settings, path)

  defp parse_standalone_example(
         "example of the html form",
         div,
         block_parse_settings,
         [section(id: "checkout"), section(id: "internet_acquiring")] = path
       ),
       do: parse_standalone_example(true, div, block_parse_settings, path)

  defp parse_standalone_example(
         "example of data about products",
         div,
         block_parse_settings,
         [endpoint(id: "checkout"), section(id: "internet_acquiring")] = path
       ),
       do: parse_standalone_example(true, div, block_parse_settings, path)

  defp parse_standalone_example(
         "connection",
         div,
         block_parse_settings,
         [section(id: "widget"), section(id: "internet_acquiring")] = path
       ),
       do: parse_standalone_example(true, div, block_parse_settings, path)

  defp parse_standalone_example(
         "example js widget",
         _div,
         _block_parse_settings,
         [section(id: "widget"), section(id: "internet_acquiring")] = _path
       ),
       do: :error

  defp parse_standalone_example(
         "example of data about products",
         div,
         block_parse_settings,
         [endpoint(id: "card_payment"), section(id: "internet_acquiring")] = path
       ),
       do: parse_standalone_example(true, div, block_parse_settings, path)

  defp parse_standalone_example(
         "example of data about products",
         div,
         block_parse_settings,
         [endpoint(id: "complete"), section(id: "two_step"), section(id: "internet_acquiring")] =
           path
       ),
       do: parse_standalone_example(true, div, block_parse_settings, path)

  defp parse_standalone_example(
         "example of getting compensation_id",
         div,
         block_parse_settings,
         [
           endpoint(id: "compensation_per_day"),
           section(id: "register"),
           section(id: "information")
         ] = path
       ),
       do: parse_standalone_example(false, div, block_parse_settings, path)

  defp parse_standalone_example(
         "example of json request",
         div,
         block_parse_settings,
         [_, section(id: "register"), section(id: "information")] = path
       ),
       do: parse_standalone_example(true, div, block_parse_settings, path)

  defp parse_standalone_example(
         "example of json response",
         div,
         block_parse_settings,
         [endpoint(), section(id: "register"), section(id: "information")] = path
       ),
       do: parse_standalone_example(false, div, block_parse_settings, path)

  defp parse_standalone_example(
         "example of the request",
         div,
         block_parse_settings,
         [_, section(id: "public")] = path
       ),
       do: parse_standalone_example(true, div, block_parse_settings, path)

  defp parse_standalone_example(
         "response example with failed status",
         div,
         block_parse_settings,
         [_, section(id: "public")] = path
       ),
       do: parse_standalone_example(false, div, block_parse_settings, path)

  defp parse_standalone_example(
         is_request,
         div,
         block_parse_settings(
           code_block_language_panel_class: code_block_language_panel_class,
           code_block_language_panel_menu_class: code_block_language_panel_menu_class,
           code_block_language_panel_menu_item_class: code_block_language_panel_menu_item_class
         ) = block_parse_settings,
         _path
       )
       when is_boolean(is_request) and is_binary(code_block_language_panel_class) and
              is_binary(code_block_language_panel_menu_class) and
              is_binary(code_block_language_panel_menu_item_class) do
    languages =
      div
      |> Floki.find(
        "div.#{code_block_language_panel_class} div.#{code_block_language_panel_menu_class} a.#{code_block_language_panel_menu_item_class}"
      )
      |> Enum.map(fn node ->
        node |> parse_node_text(block_parse_settings) |> String.downcase()
      end)
      |> MapSet.new()

    cond do
      MapSet.member?(languages, "json") ->
        [code_string] = Floki.find(div, "code.language-json")
        code = code_string |> extract_code() |> Jason.decode!()
        {:ok, {is_request, code}}

      MapSet.member?(languages, "nodejs") ->
        [code_string] = Floki.find(div, "code.language-javascript")

        [[code_string]] =
          code_string
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

      :else ->
        :error
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
        if inline_code_text_class &&
             span |> node_classes() |> Enum.member?(inline_code_text_class) do
          "`#{text}`"
        else
          text
        end

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
