if Mix.env() in [:dev] do
  defmodule LiqPayAPI.Generator.Renderer do
    use OpenAPIClient.Generator.Renderer
    alias OpenAPI.Processor.Operation
    alias OpenAPI.Renderer.File

    @impl OpenAPI.Renderer
    def render_type(_state, {:string, "date-time-liqpay"}), do: quote(do: DateTime.t())
    def render_type(_state, {:string, "date-liqpay"}), do: quote(do: Date.t())
    def render_type(_state, {:string, "month-year-liqpay"}), do: quote(do: Date.t())
    def render_type(_state, {:integer, "timestamp-ms"}), do: quote(do: DateTime.t())
    def render_type(_state, {:string, "boolean-integer"}), do: quote(do: boolean())
    def render_type(_state, {:string, "boolean-yesno"}), do: quote(do: boolean())
    def render_type(state, type), do: OpenAPIClient.Generator.Renderer.render_type(state, type)

    @impl OpenAPI.Renderer
    def write(state, %OpenAPI.Renderer.File{contents: contents} = file) do
      contents_new =
        contents
        |> IO.iodata_to_binary()
        |> String.replace(
          ~r/("(?:[\w\d]+:\/\/[\w\d]+(?:\.[\w\d]+))?(?:\/[\w\d]+)+)\?[^"]+(")/,
          "\\1\\2"
        )

      file_new = %OpenAPI.Renderer.File{file | contents: contents_new}
      OpenAPIClient.Generator.Renderer.write(state, file_new)
    end

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
    def render_operation_function(state, operation) do
      state
      |> OpenAPIClient.Generator.Renderer.render_operation_function(operation)
      |> Macro.prewalk(fn
        {:@, _attribute_metadata, [{:base_url, _base_url_metadata, _base_url_context}]} =
            base_url_attribute ->
          case operation do
            %Operation{request_path: "/ratenbu.php", request_method: :get} ->
              "https://api.buh.privatbank.ua"

            _ ->
              base_url_attribute
          end

        expression ->
          expression
      end)
    end
  end
end
