defmodule Circuits.I2C.SHT3x.MixProject do
  use Mix.Project

  def project do
    [
      app: :circuits_i2c_sht3x,
      version: "0.1.0",
      elixir: "~> 1.5",
      start_permanent: Mix.env() == :prod,
      deps: deps(),
    ]
  end

  def application do
    [
      extra_applications: [:logger]
    ]
  end

  defp deps do
    [
      {:circuits_i2c, "~> 0.3.6"},
      {:crc, "~> 0.10.0"},
    ]
  end
end
