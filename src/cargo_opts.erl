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
    release/2,
    target_dir/1,
    target_dir/2
]).

-record(opts, {
    path :: file:name_all(),
    release :: boolean(),
    toolchain :: binary() | undefined,
    target :: binary() | undefined,
    target_dir :: binary() | undefined
}).

-type params() :: #{
    path := file:name_all(),
    release := boolean(),
    toolchain := binary(),
    target := binary(),
    target_dir := binary()
}.

-opaque t() :: #opts{}.

%% @private
-spec new(params()) -> t().
new(Opts) ->
    Path = maps:get(path, Opts),
    Release = maps:get(release, Opts, false),
    Toolchain = maps:get(toolchain, Opts, undefined),
    Target = maps:get(target, Opts, undefined),
    TargetDir = maps:get(target_dir, Opts, undefined),

    #opts{
        path=Path,
        release=Release,
        toolchain=Toolchain,
        target=Target,
        target_dir=TargetDir
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

-spec target_dir(t()) -> binary() | undefined.
target_dir(#opts{target_dir=TargetDir}) ->
    TargetDir.

-spec target_dir(t(), binary()) -> t().
target_dir(O, TargetDir) ->
    O#opts{target_dir=TargetDir}.

