ExUnit.start()
Mox.defmock(OpenAPIClient.HTTPoisonMock, for: HTTPoison.Base)
Mox.defmock(OpenAPIClientMock, for: OpenAPIClient)
Mox.defmock(LiqPayAPI.CallbacksMock, for: LiqPayAPI.Callbacks)
