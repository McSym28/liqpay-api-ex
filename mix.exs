defmodule LiqpayAPI.MixProject do
  use Mix.Project

  def project do
    [
      app: :liqpay_api_ex,
      version: "0.1.0",
      elixir: "~> 1.16",
      start_permanent: Mix.env() == :prod,
      deps: deps(),
      dialyzer: [plt_add_apps: [:mix, :wallaby]]
    ]
  end

  # Run "mix help compile.app" to learn about applications.
  def application do
    [
      extra_applications: [:logger, :jason, :httpoison]
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

  defp opts_for_open_api_client_ex(env) when env in [:dev, :test],
    do: [{:env, :dev} | opts_for_open_api_client_ex(:prod)]

  defp opts_for_open_api_client_ex(_env),
    do: [git: "../../../open-api-client-ex", ref: "f804920c405dd3f39a9e7c22d7494045f6f43bea"]
end
