defmodule LiqpayAPI.MixProject do
  use Mix.Project

  def project do
    [
      app: :liqpay_api_ex,
      version: "0.1.0",
      elixir: "~> 1.16",
      start_permanent: Mix.env() == :prod,
      deps: deps()
    ]
  end

  # Run "mix help compile.app" to learn about applications.
  def application do
    [
      extra_applications: [:logger]
    ]
  end

  # Run "mix help deps" to learn about dependencies.
  defp deps do
    [
      {:open_api_client_ex, opts_for_open_api_client_ex(Mix.env())},
      {:jason, "~> 1.4", optional: true, only: [:dev, :test]},
      {:httpoison, "~> 2.2", optional: true, only: [:dev, :test]},
      {:mox, "~> 1.1", only: [:dev, :test]},
      {:floki, "~> 0.36", only: [:dev, :test]},
      {:fast_html, "~> 2.0", only: [:dev, :test]},
      {:wallaby, "~> 0.30", runtime: false, only: [:dev, :test]}
    ]
  end

  defp opts_for_open_api_client_ex(:dev), do: [{:env, :dev} | opts_for_open_api_client_ex(:prod)]

  defp opts_for_open_api_client_ex(_env),
    do: [git: "../../../open-api-client-ex", ref: "5eeb3c00f7f8f779cede16fb10fca9c784d14ea0"]
end
