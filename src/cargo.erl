-module(cargo).

-export([
    init/1,
    init/2,
    metadata/1,
    build/1,
    test/1
]).

-export([
    build_and_capture/1,
    get_package_versions/1
]).

-type result() :: cargo_cmd:output().


-spec init(file:name_all()) -> cargo_opts:t().
init(Path) ->
    init(Path, #{}).


-spec init(file:name_all(), #{ atom() => _ }) -> cargo_opts:t().
init(Path, Opts) ->
    cargo_opts:new(Opts#{ path => Path }).


-spec metadata(cargo_opts:t()) -> #{ binary() => _ }.
metadata(Opts) ->
    % --release is invalid for metadata, skip
    Opts1 = cargo_opts:release(Opts, false),
    [Metadata] = cargo_cmd:run_with_flags(
        Opts1,
        "metadata",
        ["--format-version=1", "--no-deps"]
    ),
    Metadata.


-spec build(cargo_opts:t()) -> result().
build(Opts) ->
    cargo_cmd:run_with_flags(
        Opts,
        "build",
        ["--message-format=json-diagnostic-short"]
    ).


-spec test(cargo_opts:t()) -> result().
test(Opts) ->
    cargo_cmd:run_with_flags(
        Opts,
        "test",
        ["--message-format=json-diagnostic-short",
         "--", "-Z", "unstable-options", "--format=json"]
    ).


-spec get_package_versions(cargo_opts:t()) -> #{ atom() => _ }.
get_package_versions(Opts) ->
    #{<<"packages">> := Packages} = metadata(Opts),
    maps:from_list([
        {
            maps:get(<<"id">>, M),
            #{name => maps:get(<<"name">>, M), version => maps:get(<<"version">>, M)}
        }
        || M <- Packages
    ]).


-spec build_and_capture(cargo_opts:t()) -> [cargo_artifact:t()].
build_and_capture(Opts) ->
    Packages = get_package_versions(Opts),
    Outputs = build(Opts),

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