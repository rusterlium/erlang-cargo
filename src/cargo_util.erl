-module(cargo_util).

-export([
    ensure_binary/1
]).

-export([
    get_crate_dirs/1,
    get_priv_dir/1,
    is_dylib/1,
    is_executable/1
]).

-define(SOURCE_DIR, "rust_src").
-define(OUTPUT_DIR, "crates").


get_crate_dirs(AppDir) ->
    Tomls = filelib:wildcard([?SOURCE_DIR, "/*/Cargo.toml"], AppDir),
    AbsTomls = [ filename:absname(T, AppDir) || T <- Tomls ],
    [ filename:dirname( T) || T <- AbsTomls ].


get_priv_dir(App) ->
    % TODO: Common function for rustler_mix and rebar3_cargo
    %PrivDir = rebar_app_info:priv_dir(App),  % ensure_dir/1 fails if priv not present (ref https://github.com/erlang/rebar3/issues/1173)
    AppDir = rebar_app_info:dir(App),
    filename:join([AppDir, "priv", "crates"]).


-spec ensure_binary(string() | iolist() | binary()) -> binary().
ensure_binary(List) when is_list(List) ->
    list_to_binary(List);

ensure_binary(Binary) when is_binary(Binary) ->
    Binary.


-spec is_dylib(file:filename_all()) -> boolean().
is_dylib(Path) ->
    Ext = ensure_binary(filename:extension(Path)),
    case {Ext, os:type()} of
        {<<".dll">>, {win32, _}} ->
            true;
        {<<".dylib">>, {unix, darwin}} ->
            true;
        {<<".so">>, {unix, Os}} when Os =/= darwin ->
            true;
        _ ->
            false
    end.


-spec is_executable(file:filename_all()) -> boolean().
is_executable(Path) ->
    Ext = ensure_binary(filename:extension(Path)),
    case {Ext, os:type()} of
        {<<".exe">>, {win32, _}} ->
            true;
        {<<"">>, _} ->
            true;
        _ ->
            false
    end.