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
        ["--message-format=json-diagnostic-short"]
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


-spec build_and_capture(cargo_opts:t()) -> #{ atom() => _ }.
build_and_capture(Opts) ->
    Packages = get_package_versions(Opts),
    Outputs = build(Opts),

    Artifacts1 = lists:foldl(
        fun (Entry, Artifacts) ->
            #{
                <<"reason">> := Reason,
                <<"package_id">> := PackageId
            } = Entry,

            case Reason of
                <<"compiler-artifact">> when is_map_key(PackageId, Artifacts) ->
                    Current = maps:get(PackageId, Artifacts),
                    New = Current#{
                        fresh => maps:get(<<"fresh">>, Entry, false),
                        filenames => maps:get(<<"filenames">>, Entry),
                        kind => maps:get(<<"kind">>, maps:get(<<"target">>, Entry))
                    },
                    Artifacts#{ PackageId => New};
                _ ->
                    Artifacts
            end
        end,
        Packages,
        Outputs
    ),

    Artifacts1.