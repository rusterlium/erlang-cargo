-module(cargo_util).

-export([
    ensure_binary/1
]).

-export([
    is_dylib/1,
    is_executable/1
]).

-define(SOURCE_DIR, "rust_src").
-define(OUTPUT_DIR, "crates").


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