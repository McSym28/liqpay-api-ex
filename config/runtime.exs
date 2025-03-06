import Config

if config_env() in [:dev, :prod] do
  config :liqpay_api_ex,
    private_key:
      System.get_env("LIQPAY_API_PRIVATE_KEY") ||
        raise("environment variable LIQPAY_API_PRIVATE_KEY is missing."),
    public_key:
      System.fetch_env!("LIQPAY_API_PUBLIC_KEY") ||
        raise("environment variable LIQPAY_API_PUBLIC_KEY is missing.")
end
