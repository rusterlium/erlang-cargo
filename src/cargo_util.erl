-module(cargo_util).

-export([
    package_flag/1,
    ensure_binary/1
]).

-export([
    is_dylib/1,
    is_executable/1
]).

-export_type([
    to_binary/0,
    maybe_package/0
]).

-type to_binary() :: atom() | string() | iolist() | binary().
-type maybe_package() :: {true, to_binary()} | false.

-define(SOURCE_DIR, "rust_src").
-define(OUTPUT_DIR, "crates").

-spec package_flag(maybe_package()) -> [binary()].
package_flag({true, Package}) ->
    [<<"-p">>, ensure_binary(Package)];
package_flag(false) ->
    [].

-spec ensure_binary(to_binary()) -> binary().
ensure_binary(Atom) when is_atom(Atom) ->
    atom_to_binary(Atom, utf8);
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
