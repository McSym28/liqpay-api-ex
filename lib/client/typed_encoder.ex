defmodule LiqPayAPI.Client.TypedEncoder do
  alias OpenAPIClient.Client.TypedEncoder
  alias OpenAPIClient.Client.Error
  alias OpenAPIClient.Utils

  @nested_clauses [
    {LiqPayAPI.Tokens.Obtain.Request, :t,
     ~w(card_tokenization connect_control_tokenization vceh_tokenization)a}
  ]

  @behaviour TypedEncoder

  @impl TypedEncoder
  @doc """
  Encode a value of a specific type

  ## Examples

    iex> #{__MODULE__}.encode(~U[2024-05-06T11:22:33Z], {:string, "date-time-liqpay"}, [], #{__MODULE__})
    {:ok, "2024-05-06 11:22:33"}
    iex> #{__MODULE__}.encode(~D[2023-02-01], {:string, "date-liqpay"}, [], #{__MODULE__})
    {:ok, "01.02.2023"}
    iex> #{__MODULE__}.encode(~D[2023-02-01], {:string, "month-year-liqpay"}, [], #{__MODULE__})
    {:ok, "0223"}
    iex> #{__MODULE__}.encode(~U[2024-02-01T01:23:45.987Z], {:integer, "timestamp-ms"}, [], #{__MODULE__})
    {:ok, 1706750625987}
    iex> #{__MODULE__}.encode(true, {:string, "boolean-integer"}, [], #{__MODULE__})
    {:ok, "1"}
    iex> #{__MODULE__}.encode(false, {:string, "boolean-integer"}, [], #{__MODULE__})
    {:ok, "0"}
    iex> #{__MODULE__}.encode(true, {:string, "boolean-yesno"}, [], #{__MODULE__})
    {:ok, "Y"}
    iex> #{__MODULE__}.encode(false, {:string, "boolean-yesno"}, [], #{__MODULE__})
    {:ok, "N"}

  """
  def encode(nil, _, _, _), do: {:ok, nil}

  def encode(%DateTime{} = value, {:string, "date-time-liqpay"}, _path, _caller_module) do
    {:ok, Calendar.strftime(value, "%Y-%m-%d %H:%M:%S")}
  end

  def encode(_value, {:string, "date-time-liqpay"}, path, _caller_module) do
    {:error,
     Error.new(
       message: "Invalid format for date/time value",
       reason: :invalid_datetime_value,
       source: path
     )}
  end

  def encode(%Date{} = value, {:string, "date-liqpay"}, _path, _caller_module) do
    {:ok, Calendar.strftime(value, "%d.%m.%Y")}
  end

  def encode(%DateTime{} = value, {:string, "date-liqpay"} = type, path, caller_module) do
    caller_module.encode(DateTime.to_date(value), type, path, caller_module)
  end

  def encode(_value, {:string, "date-liqpay"}, path, _caller_module) do
    {:error,
     Error.new(
       message: "Invalid format for date value",
       reason: :invalid_date_value,
       source: path
     )}
  end

  def encode(%Date{} = value, {:string, "month-year-liqpay"}, _path, _caller_module) do
    {:ok, Calendar.strftime(value, "%m%y")}
  end

  def encode(%DateTime{} = value, {:string, "month-year-liqpay"} = type, path, caller_module) do
    caller_module.encode(DateTime.to_date(value), type, path, caller_module)
  end

  def encode(_value, {:string, "month-year-liqpay"}, path, _caller_module) do
    {:error,
     Error.new(
       message: "Invalid format for month-year value",
       reason: :invalid_monthyear_value,
       source: path
     )}
  end

  def encode(%DateTime{} = value, {:integer, "timestamp-ms"}, _path, _caller_module) do
    {:ok, DateTime.to_unix(value, :millisecond)}
  end

  def encode(_value, {:integer, "timestamp-ms"}, path, _caller_module) do
    {:error,
     Error.new(
       message: "Invalid format for timestamp value",
       reason: :invalid_timestamp_value,
       source: path
     )}
  end

  def encode(true, {:string, "boolean-integer"}, _path, _caller_module), do: {:ok, "1"}
  def encode(false, {:string, "boolean-integer"}, _path, _caller_module), do: {:ok, "0"}

  def encode(_value, {:string, "boolean-integer"}, path, _caller_module) do
    {:error,
     Error.new(
       message: "Invalid format for integer boolean value",
       reason: :invalid_integer_boolean_value,
       source: path
     )}
  end

  def encode(true, {:string, "boolean-yesno"}, _path, _caller_module), do: {:ok, "Y"}
  def encode(false, {:string, "boolean-yesno"}, _path, _caller_module), do: {:ok, "N"}

  def encode(_value, {:string, "boolean-yesno"}, path, _caller_module) do
    {:error,
     Error.new(
       message: "Invalid format for yes/no boolean value",
       reason: :invalid_yesno_boolean_value,
       source: path
     )}
  end

  Enum.map(
    @nested_clauses,
    fn {module, type, nested_fields} ->
      def encode(
            value,
            {unquote(module) = module, unquote(type) = type},
            path,
            caller_module
          )
          when is_map(value) do
        value
        |> encode_schema({module, type}, path, caller_module)
        |> case do
          {:ok, map} ->
            nested_keys =
              type
              |> module.__fields__()
              |> Keyword.take(unquote(nested_fields))
              |> Enum.map(fn {_name, {old_name, _type}} -> old_name end)

            {nested_fields, map_rest} = Map.split(map, nested_keys)

            map_new =
              Enum.reduce(
                nested_fields,
                map_rest,
                fn {_key, value}, acc -> Map.merge(acc, value) end
              )

            {:ok, map_new}

          error ->
            error
        end
      end
    end
  )

  def encode(
        %module{} = value,
        {module, type},
        path,
        caller_module
      )
      when is_atom(module) and is_atom(type) do
    if Utils.is_module?(module) and Utils.does_implement_behaviour?(module, OpenAPIClient.Schema) do
      encode_schema(value, {module, type}, path, caller_module)
    else
      TypedEncoder.encode(value, type, path, caller_module)
    end
  end

  def encode(value, type, path, caller_module),
    do: TypedEncoder.encode(value, type, path, caller_module)

  defp encode_schema(
         %module{} = value,
         {module, _schema_type} = type,
         path,
         caller_module
       ) do
    case TypedEncoder.encode(value, type, path, caller_module) do
      {:ok, value_encoded} ->
        value_encoded_new = Map.reject(value_encoded, fn {_key, value} -> is_nil(value) end)
        {:ok, value_encoded_new}

      error ->
        error
    end
  end

  @spec nested_clauses() :: [{module(), atom(), [atom()]}]
  def nested_clauses, do: @nested_clauses
end
