defmodule LiqpayAPI.Client.Steps.RequestBodySigner do
  @behaviour Pluggable

  alias OpenAPI.Renderer.Operation
  alias OpenAPIClient.Client.{Error, Operation}

  @type options :: []

  @eligible_urls ["/api/request"]

  @impl Pluggable
  @spec init(options()) :: options()
  def init(opts), do: opts

  @impl Pluggable
  @spec call(Operation.t(), options()) :: Operation.t()
  def call(
        %Operation{request_url: request_url, request_method: :post, request_body: request_body} =
          operation,
        _opts
      )
      when request_url in @eligible_urls and is_binary(request_body) do
    private_key =
      operation
      |> Operation.get_private(:__params__, [])
      |> Keyword.get(:private_key)

    if is_binary(private_key) do
      form_data = LiqpayAPI.Client.Signature.generate_form_data(request_body, private_key)

      %Operation{operation | request_body: form_data}
      |> Operation.put_request_header("Content-Type", "application/x-www-form-urlencoded")
    else
      Operation.set_result(
        operation,
        {:error,
         Error.new(
           message: "`private_key` not set",
           operation: operation,
           reason: :private_key_not_set,
           step: __MODULE__
         )}
      )
    end
  end

  def call(operation, _opts), do: operation
end
