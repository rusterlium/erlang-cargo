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


init(Path) ->
    init(Path, #{}).

init(Path, Opts) ->
    cargo_opts:new(Opts#{ path => Path }).


metadata(Opts) ->
    {ok, [Metadata]} = cargo_cmd:run(
        Opts,
        ["metadata", "--format-version=1", "--no-deps"]
    ),
    Metadata.


build(Opts) ->
    cargo_cmd:run(
        Opts,
        ["build", "--message-format=json-diagnostic-short"]
    ).


test(Opts) ->
    cargo_cmd:run(
        Opts,
        ["test", "--message-format=json-diagnostic-short"]
    ).


get_package_versions(Opts) ->
    #{<<"packages">> := Packages} = metadata(Opts),
    maps:from_list([
        {
            maps:get(<<"id">>, M),
            #{name => maps:get(<<"name">>, M), version => maps:get(<<"version">>, M)}
        }
        || M <- Packages
    ]).


build_and_capture(Opts) ->
    Packages = get_package_versions(Opts),
    {ok, Outputs} = build(Opts),

    Artifacts = lists:foldl(
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
                        filenames => maps:get(<<"filenames">>, Entry)
                    },
                    Artifacts#{ PackageId => New};
                _ ->
                    Artifacts
            end
        end,
        Packages,
        Outputs
    ),

    Artifacts.