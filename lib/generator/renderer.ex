if Mix.env() in [:dev] do
  defmodule LiqPayAPI.Generator.Renderer do
    use OpenAPIClient.Generator.Renderer

    @impl OpenAPI.Renderer
    def render_type(_state, {:string, "date-time-liqpay"}), do: quote(do: DateTime.t())
    def render_type(_state, {:string, "date-liqpay"}), do: quote(do: Date.t())
    def render_type(_state, {:string, "month-year-liqpay"}), do: quote(do: Date.t())
    def render_type(_state, {:integer, "timestamp-ms"}), do: quote(do: DateTime.t())
    def render_type(_state, {:string, "boolean-integer"}), do: quote(do: boolean())
    def render_type(_state, {:string, "boolean-yesno"}), do: quote(do: boolean())
    def render_type(state, type), do: OpenAPIClient.Generator.Renderer.render_type(state, type)

    @impl OpenAPI.Renderer
    def write(state, %OpenAPI.Renderer.File{contents: contents, location: location} = file) do
      if String.ends_with?(location, "_test.exs") do
        OpenAPIClient.Generator.Renderer.write(state, file)
      else
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
    end
  end
end
