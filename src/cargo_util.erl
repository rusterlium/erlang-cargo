-module(cargo_util).

-export([
    ensure_binary/1
]).

-export([
    get_crate_dirs/1,
    get_priv_dir/1,
    check_extension/2
]).

-define(SOURCE_DIR, "rust_src").
-define(OUTPUT_DIR, "crates").


get_crate_dirs(AppDir) ->
    Tomls = filelib:wildcard([?SOURCE_DIR, "/*/Cargo.toml"], AppDir),
    AbsTomls = [ filename:absname(T, AppDir) || T <- Tomls ],
    [ filename:dirname( T) || T <- AbsTomls ].


get_priv_dir(App) ->
    %PrivDir = rebar_app_info:priv_dir(App),  % ensure_dir/1 fails if priv not present (ref https://github.com/erlang/rebar3/issues/1173)
    AppDir = rebar_app_info:dir(App),
    filename:join([AppDir, "priv", "crates"]).


ensure_binary(List) when is_list(List) ->
    list_to_binary(List);

ensure_binary(Binary) when is_binary(Binary) ->
    Binary.


-spec check_extension(binary(), {atom(), atom()}) -> boolean().
check_extension(<<".dll">>, {win32, _}) -> true;
check_extension(<<".dylib">>, {unix, darwin}) -> true;
check_extension(<<".so">>, {unix, Os}) when Os =/= darwin -> true;
check_extension(_, _) -> false.
