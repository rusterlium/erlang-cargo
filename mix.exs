defmodule Cargo.MixProject do
  use Mix.Project

  def project do
    {:ok, rebar_config} = :file.consult("rebar.config")

    {:ok, [{_, _, app_src}]} = :file.consult("src/cargo.app.src")
    version = :erlang.list_to_binary(Keyword.get(app_src, :vsn))
    deps = Keyword.get(rebar_config, :deps)

    [
      app: :cargo,
      version: version,
      deps: deps
    ]
  end
end
