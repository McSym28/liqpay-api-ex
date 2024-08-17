defmodule LiqPayAPI.Client.TypedDecoder do
  alias OpenAPIClient.Client.TypedDecoder
  alias OpenAPIClient.Client.Error

  @behaviour TypedDecoder

  @impl TypedDecoder
  @doc """
  Decode a value of a specific type

  ## Examples

    iex> #{__MODULE__}.decode("2024-05-06 11:22:33", {:string, "date-time-liqpay"}, [], #{__MODULE__})
    {:ok, ~U[2024-05-06T11:22:33Z]}
    iex> #{__MODULE__}.decode("01.02.2023", {:string, "date-liqpay"}, [], #{__MODULE__})
    {:ok, ~D[2023-02-01]}
    iex> #{__MODULE__}.decode("0223", {:string, "month-year-liqpay"}, [], #{__MODULE__})
    {:ok, ~D[2023-02-01]}
    iex> #{__MODULE__}.decode(1706750625987, {:integer, "timestamp-ms"}, [], #{__MODULE__})
    {:ok, ~U[2024-02-01T01:23:45.987Z]}
    iex> #{__MODULE__}.decode("1", {:string, "boolean-integer"}, [], #{__MODULE__})
    {:ok, true}
    iex> #{__MODULE__}.decode("0", {:string, "boolean-integer"}, [], #{__MODULE__})
    {:ok, false}
    iex> #{__MODULE__}.decode("Y", {:string, "boolean-yesno"}, [], #{__MODULE__})
    {:ok, true}
    iex> #{__MODULE__}.decode("N", {:string, "boolean-yesno"}, [], #{__MODULE__})
    {:ok, false}

  """
  def decode(nil, _, _, _), do: {:ok, nil}

  def decode(
        <<y1::utf8, y2::utf8, y3::utf8, y4::utf8, "-", m1::utf8, m2::utf8, "-", d1::utf8,
          d2::utf8, " ", h1::utf8, h2::utf8, ":", min1::utf8, min2::utf8, ":", s1::utf8,
          s2::utf8>>,
        {:string, "date-time-liqpay"},
        path,
        caller_module
      ) do
    <<y1::utf8, y2::utf8, y3::utf8, y4::utf8, "-", m1::utf8, m2::utf8, "-", d1::utf8, d2::utf8,
      "T", h1::utf8, h2::utf8, ":", min1::utf8, min2::utf8, ":", s1::utf8, s2::utf8, "Z">>
    |> caller_module.decode({:string, :date_time}, path, caller_module)
  end

  def decode(value, {:string, "date-time-liqpay"}, path, caller_module) do
    caller_module.decode(value, {:string, :date_time}, path, caller_module)
  end

  def decode(
        <<d1::utf8, d2::utf8, ".", m1::utf8, m2::utf8, ".", y1::utf8, y2::utf8, y3::utf8,
          y4::utf8>>,
        {:string, "date-liqpay"},
        path,
        caller_module
      ) do
    <<y1::utf8, y2::utf8, y3::utf8, y4::utf8, "-", m1::utf8, m2::utf8, "-", d1::utf8, d2::utf8>>
    |> caller_module.decode({:string, :date}, path, caller_module)
  end

  def decode(value, {:string, "date-liqpay"}, path, caller_module) do
    caller_module.decode(value, {:string, :date}, path, caller_module)
  end

  def decode(
        <<m1::utf8, m2::utf8, y3::utf8, y4::utf8>>,
        {:string, "month-year-liqpay"},
        path,
        caller_module
      ) do
    <<"20", y3::utf8, y4::utf8, "-", m1::utf8, m2::utf8, "-01">>
    |> caller_module.decode({:string, :date}, path, caller_module)
  end

  def decode(_value, {:string, "month-year-liqpay"}, path, _caller_module) do
    {:error,
     Error.new(
       message: "Invalid format for month-year value",
       reason: :invalid_monthyear_string,
       source: path
     )}
  end

  def decode(value, {:integer, "timestamp-ms"}, path, _caller_module) when is_integer(value) do
    value
    |> DateTime.from_unix(:millisecond)
    |> case do
      {:ok, value_decoded} ->
        {:ok, value_decoded}

      {:error, reason} ->
        {:error,
         Error.new(
           message: "Error while decoding timestamp value",
           reason: reason,
           source: path
         )}
    end
  end

  def decode(_value, {:integer, "timestamp-ms"}, path, _caller_module) do
    {:error,
     Error.new(
       message: "Invalid format for timestamp value",
       reason: :invalid_timestamp_value,
       source: path
     )}
  end

  def decode("1", {:string, "boolean-integer"}, _path, _caller_module), do: {:ok, true}
  def decode("0", {:string, "boolean-integer"}, _path, _caller_module), do: {:ok, false}

  def decode(_value, {:string, "boolean-integer"}, path, _caller_module) do
    {:error,
     Error.new(
       message: "Invalid format for string integer boolean value",
       reason: :invalid_integer_boolean_string,
       source: path
     )}
  end

  def decode(value, {:string, "boolean-yesno"}, _path, _caller_module) when value in ~w(Y y),
    do: {:ok, true}

  def decode(value, {:string, "boolean-yesno"}, _path, _caller_module) when value in ~w(N n),
    do: {:ok, false}

  def decode(_value, {:string, "boolean-yesno"}, path, _caller_module) do
    {:error,
     Error.new(
       message: "Invalid format for string yes/no boolean value",
       reason: :invalid_yesno_boolean_string,
       source: path
     )}
  end

  Enum.map(
    LiqPayAPI.Client.TypedEncoder.nested_clauses(),
    fn {module, type, nested_fields} ->
      def decode(
            value,
            {unquote(module) = module, unquote(type) = type},
            path,
            caller_module
          ) do
        value_new =
          type
          |> module.__fields__()
          |> Keyword.take(unquote(nested_fields))
          |> Enum.reduce(value, fn {_name, {old_name, {nested_module, nested_type}}}, acc ->
            nested_keys =
              nested_type
              |> nested_module.__fields__()
              |> Enum.map(fn {_name, {old_name, _type}} -> old_name end)

            {nested_fields, acc_rest} = Map.split(acc, nested_keys)
            Map.put(acc_rest, old_name, nested_fields)
          end)

        TypedDecoder.decode(value_new, type, path, caller_module)
      end
    end
  )

  def decode(value, type, path, caller_module),
    do: TypedDecoder.decode(value, type, path, caller_module)
end
