if Code.ensure_loaded?(HTTPoison) do
  defmodule LiqpayAPI.Client.BasicHTTPoisonPipeline do
    use Pluggable.StepBuilder

    step(OpenAPIClient.Client.Steps.RequestBodyTypedEncoder)
    step(OpenAPIClient.Client.Steps.RequestBodyContentTypeEncoder)
    step(LiqpayAPI.Client.Steps.RequestBodySigner)
    step(OpenAPIClient.Client.Steps.HTTPoisonClient)
    step(OpenAPIClient.Client.Steps.ResponseBodyContentTypeDecoder)
    step(OpenAPIClient.Client.Steps.ResponseBodyTypedDecoder)
  end
end
