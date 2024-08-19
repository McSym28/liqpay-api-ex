import Config

config :liqpay_api_ex,
  private_key: System.get_env("LIQPAY_API_PRIVATE_KEY"),
  public_key: System.get_env("LIQPAY_API_PUBLIC_KEY")
