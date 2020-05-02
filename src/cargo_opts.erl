-module(cargo_opts).

-export_type([
    params/0,
    t/0
]).

-export([
    new/1,
    toolchain/1,
    path/1,
    target/1,
    release/1,
    release/2
]).

-record(opts, {
    path :: file:name_all(),
    release :: boolean(),
    toolchain :: binary() | undefined,
    target :: binary() | undefined
}).

-type params() :: #{
    path := file:name_all(),
    release := boolean(),
    toolchain := binary(),
    target := binary()
}.

-opaque t() :: #opts{}.

%% @private
-spec new(params()) -> t().
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

-spec toolchain(t()) -> binary() | undefined.
toolchain(#opts{toolchain=Toolchain}) ->
    Toolchain.

-spec release(t()) -> boolean().
release(#opts{release=Release}) ->
    Release.

-spec release(t(), boolean()) -> t().
release(O, Release) ->
    O#opts{release=Release}.

-spec path(t()) -> file:filename_all().
path(#opts{path=Path}) ->
    Path.

-spec target(t()) -> binary() | undefined.
target(#opts{target=Target}) ->
    Target.