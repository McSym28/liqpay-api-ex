if Mix.env() in [:dev] do
  defmodule LiqpayAPI.Generator.Renderer do
    use OpenAPIClient.Generator.Renderer

    @impl OpenAPI.Renderer
    def render_type(_state, {:integer, "timestamp-ms"}), do: quote(do: DateTime.t())
    def render_type(_state, {:string, "date-time-liqpay"}), do: quote(do: DateTime.t())
    def render_type(_state, {:string, "date-liqpay"}), do: quote(do: Date.t())
    def render_type(_state, {:string, "month-year-liqpay"}), do: quote(do: Date.t())
    def render_type(_state, {:string, "boolean-integer"}), do: quote(do: boolean())
    def render_type(_state, {:string, "boolean-yesno"}), do: quote(do: boolean())
    def render_type(state, type), do: OpenAPI.Renderer.render_type(state, type)
  end
end
