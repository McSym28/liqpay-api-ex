defmodule LiqPayAPI.Plugs.CallbackSignatureChecker do
  @moduledoc """
  A plug for checking callback signature.

  This plug MUST be called after the `OpenAPIClient.Plugs.CallbackInitializer` plug
  (to initialize `:open_api_client_ex` state).
  Add `Plug.Parsers` plug with `parsers: [:json]` after this to parse fetched JSON.

  ## Options:
  * `:body_reader` - MFA to read request body. By default the `OpenAPIClient.read_body_params/2` is used
    (make sure to call `Plug.Parsers` plug with `parsers: [:urlencoded]`.
  * `:private_key_reader` - Function to read private key used for signature verification. Defaults to
    `Application.fetch_env(:liqpay_api_ex, :private_key)`.

  """

  @behaviour Plug

  alias OpenAPIClient.Error

  @type option ::
          {:body_reader, {module(), atom(), list()}}
          | {:private_key_reader,
             {module(), atom(), list()}
             | (-> {:ok, String.t()} | :error)}
  @type options :: [option()]

  @impl Plug
  @spec init(options()) :: Plug.opts()
  def init(opts) do
    {body_reader, opts} =
      Keyword.pop(opts, :body_reader, {OpenAPIClient, :read_body_params, []})

    {body_reader, opts}
  end

  @impl Plug
  def call(conn, {body_reader, opts}) do
    %OpenAPIClient.State{} = state = OpenAPIClient.get_state(conn)

    with {:ok, private_key} <- get_private_key(conn, opts),
         {:ok, body_encoded, conn} <-
           read_body(conn, body_reader, opts),
         {:ok, body} <- decode_body(conn, body_encoded),
         {:ok, data_encoded} <- read_body_data(conn, body),
         {:ok, data} <- decode_data(conn, data_encoded),
         {:ok, signature} <- read_body_signature(conn, body),
         :ok <- check_signature(conn, data_encoded, private_key, signature) do
      state_new = %OpenAPIClient.State{state | request_body: data}

      %Plug.Conn{conn | body_params: %Plug.Conn.Unfetched{aspect: :body_params}}
      |> OpenAPIClient.set_state(state_new)
      |> Plug.Conn.put_req_header("content-type", "application/json")
    end
  end

  defp get_private_key(conn, opts) do
    opts
    |> Keyword.get(
      :private_key_reader,
      {Application, :fetch_env, [:liqpay_api_ex, :private_key]}
    )
    |> read_private_key()
    |> case do
      :error ->
        raise Error.new(
                message: "Private key not set",
                reason: :private_key_not_set,
                conn: conn,
                plug: __MODULE__
              )

      {:ok, private_key} ->
        {:ok, private_key}
    end
  end

  defp read_body(conn, {mod, fun, args}, opts) do
    case apply(mod, fun, [conn, opts | args]) do
      {:ok, body, conn} ->
        {:ok, body, conn}

      {:more, _, conn} ->
        raise Error.new(
                message: "Request body is too large",
                reason: :request_body_too_large,
                conn: conn,
                plug: __MODULE__
              )

      {:error, reason} ->
        raise Error.new(
                message: "Request body read error",
                reason: :request_body_read_error,
                source: reason,
                conn: conn,
                plug: __MODULE__
              )
    end
  end

  defp decode_body(conn, body) when is_binary(body) do
    body_decoded = URI.decode_query(body, %{}, :www_form)
    decode_body(conn, body_decoded)
  end

  defp decode_body(_conn, body) when is_map(body) do
    {:ok, body}
  end

  defp decode_body(conn, _body) do
    raise Error.new(
            message: "Incorrect request body",
            reason: :incorrect_request_body,
            conn: conn,
            plug: __MODULE__
          )
  end

  defp read_body_signature(conn, body) do
    body
    |> Map.fetch("signature")
    |> case do
      {:ok, signature} ->
        {:ok, signature}

      :error ->
        raise Error.new(
                message: "`signature` not found",
                reason: :signature_not_found,
                conn: conn,
                plug: __MODULE__
              )
    end
  end

  defp read_body_data(conn, body) do
    body
    |> Map.fetch("data")
    |> case do
      {:ok, data} ->
        {:ok, data}

      :error ->
        raise Error.new(
                message: "`data` not found",
                reason: :data_not_found,
                conn: conn,
                plug: __MODULE__
              )
    end
  end

  defp decode_data(conn, data) do
    data
    |> Base.decode64()
    |> case do
      {:ok, decoded} ->
        {:ok, decoded}

      :error ->
        raise Error.new(
                message: "`data` decode failed",
                reason: :data_decode_failed,
                conn: conn,
                plug: __MODULE__
              )
    end
  end

  defp check_signature(conn, data, private_key, signature) do
    data
    |> LiqPayAPI.Client.Signature.check?(private_key, signature)
    |> if do
      :ok
    else
      raise Error.new(
              message: "Signature verification failed",
              reason: :signature_verification_error,
              conn: conn,
              plug: __MODULE__
            )
    end
  end

  defp read_private_key(fun) when is_function(fun, 0), do: fun.()
  defp read_private_key({module, function, args}), do: apply(module, function, args)
end
