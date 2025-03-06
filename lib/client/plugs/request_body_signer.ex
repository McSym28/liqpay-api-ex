defmodule LiqPayAPI.Client.Plugs.RequestBodySigner do
  @moduledoc """
  A plug for addding signature to the request.
  """

  @behaviour Plug

  alias OpenAPIClient.Error

  @type options :: []

  @eligible_request_paths ["/api/request"]

  @impl Plug
  @spec init(options()) :: Plug.opts()
  def init(opts), do: opts

  @impl Plug
  def call(conn, _opts) do
    conn
    |> OpenAPIClient.get_state()
    |> case do
      %OpenAPIClient.State{
        request_path: request_path,
        method: :post,
        request_body: request_body,
        function_opts: function_opts
      } =
          state
      when request_path in @eligible_request_paths and is_binary(request_body) ->
        function_opts
        |> Keyword.fetch(:private_key)
        |> case do
          {:ok, private_key} when is_binary(private_key) ->
            form_data = LiqPayAPI.Client.Signature.generate_form_data(request_body, private_key)

            conn
            |> OpenAPIClient.set_state(%OpenAPIClient.State{state | request_body: form_data})
            |> Plug.Conn.put_req_header("content-type", "application/x-www-form-urlencoded")

          _ ->
            OpenAPIClient.set_state_result(
              conn,
              {:error,
               Error.new(
                 message: "`private_key` not set",
                 state: state,
                 reason: :private_key_not_set,
                 conn: conn,
                 plug: __MODULE__
               )}
            )
        end

      _ ->
        conn
    end
  end
end
