-module(cargo_opts).

-export_type([
    t/0
]).

-export([
    new/1,
    toolchain/1,
    path/1,
    target/1,
    release/1
]).

-record(opts, {
    path :: file:name_all(),
    release :: boolean(),
    toolchain :: binary() | undefined,
    target :: binary() | undefined
}).


-opaque t() :: #opts{}.


new(Opts) ->
    Path = maps:get(path, Opts),
    Release = maps:get(release, Opts, false),
    Toolchain = maps:get(toolchain, Opts, undefined),
    Target = maps:get(target, Opts, undefined),

    #opts{
        path=Path,
        release=Release,
        toolchain=Toolchain,
        target=Target
    }.


toolchain(#opts{toolchain=Toolchain}) ->
    Toolchain.

release(#opts{release=Release}) ->
    Release.

path(#opts{path=Path}) ->
    Path.

target(#opts{target=Target}) ->
    Target.