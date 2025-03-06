defmodule LiqPayAPIWeb.Router do
  use LiqPayAPIWeb, :router

  scope "/__test__", LiqPayAPIWeb do
    scope "/callbacks", Callbacks do
      scope "/callback" do
        post("/", CallbackController, :callback)
      end
    end
  end
end
