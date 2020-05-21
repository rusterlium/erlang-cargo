-module(cargo).

-export([
    init/1,
    init/2,
    metadata/1,

    build_raw/2,

    build/2,
    build_all/1,

    test/2,
    test_all/1,

    clean/1
]).

-export([
    get_package_versions/1
]).

-type result() :: cargo_cmd:output().


-spec init(file:name_all()) -> cargo_opts:t() | no_return().
init(Path) ->
    init(Path, #{}).


-spec init(file:name_all(), #{ atom() => _ }) -> cargo_opts:t() | no_return().
init(Path, Opts) ->
    cargo_opts:new(Opts#{ path => Path }).


-spec metadata(cargo_opts:t()) -> #{ binary() => _ } | no_return().
metadata(Opts) ->
    % --release is invalid for metadata, skip
    Opts1 = cargo_opts:release(Opts, false),
    [Metadata] = cargo_cmd:run_with_flags(
        Opts1,
        "metadata",
        ["--format-version=1", "--no-deps"]
    ),
    Metadata.


-spec build_raw(cargo_opts:t(), cargo_util:maybe_package()) -> result() | no_return().
build_raw(Opts, MaybePackage) ->
    cargo_cmd:run_with_flags(
        Opts,
        "build",
        ["--message-format=json-diagnostic-short"] ++
        cargo_util:package_flag(MaybePackage)
    ).


-spec test_all(cargo_opts:t()) -> result() | no_return().
test_all(Opts) ->
    do_test(Opts, false).

-spec test(cargo_opts:t(), cargo_util:to_binary()) -> result() | no_return().
test(Opts, Package) ->
    do_test(Opts, {true, Package}).

do_test(Opts, MaybePackage) ->
    cargo_cmd:run_with_flags(
        Opts,
        "test",
        ["--message-format=json-diagnostic-short"] ++
        cargo_util:package_flag(MaybePackage) ++
        ["--", "-Z", "unstable-options", "--format=json"]
    ).


-spec clean(cargo_opts:t()) -> ok | no_return().
clean(Opts) ->
    cargo_cmd:run_with_flags(
        Opts,
        "clean",
        ["-q"]
    ),
    ok.


-spec get_package_versions(cargo_opts:t()) -> #{ atom() => _ }.
get_package_versions(Opts) ->
    #{<<"packages">> := Packages} = metadata(Opts),
    maps:from_list([
        {
            maps:get(<<"id">>, M),
            #{
                name => maps:get(<<"name">>, M),
                version => maps:get(<<"version">>, M)
            }
        }
        || M <- Packages
    ]).


-spec build_all(cargo_opts:t()) -> [cargo_artifact:t()].
build_all(Opts) ->
    do_build(Opts, false).


-spec build(cargo_opts:t(), cargo_util:to_binary()) -> [cargo_artifact:t()].
build(Opts, Package) ->
    do_build(Opts, {true, Package}).


do_build(Opts, MaybePackage) ->
    Packages = get_package_versions(Opts),
    Outputs = build_raw(Opts, MaybePackage),

    lists:filtermap(
        fun (Entry) ->
            case cargo_artifact:from_json(Entry) of
                {ok, Artifact} ->
                    PackageId = cargo_artifact:package_id(Artifact),
                    case maps:find(PackageId, Packages) of
                        {ok, #{version := Version}} ->
                            {true, cargo_artifact:version(Artifact, Version)};
                        error ->
                            false
                    end;
                error ->
                    false
            end
        end,
        Outputs
    ).
