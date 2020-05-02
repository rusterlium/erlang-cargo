-module(cargo_SUITE).
-compile([export_all, nowarn_export_all]).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

all() ->
    [
        simple_bin,
        simple_lib,
        multiple_bins,
        package
    ].


simple_bin(Config) ->
    C = cargo:init(?config(crate_path, Config)),
    [Artifact] = cargo:build_and_capture(C),
    basic_tests(<<"simple_bin">>, <<"0.1.0">>, Artifact),
    basic_tests_bin(<<"simple_bin">>, Artifact),
    cargo:test(C),

    % ok = cargo:clean(C)
    ok.


simple_lib(Config) ->
    C = cargo:init(?config(crate_path, Config)),
    [Artifact] = cargo:build_and_capture(C),
    basic_tests(<<"simple_lib">>, <<"0.1.0">>, Artifact),
    basic_tests_lib(<<"simple_lib">>, Artifact),
    cargo:test(C),

    % ok = cargo:clean(C)
    ok.


multiple_bins(Config) ->
    C = cargo:init(?config(crate_path, Config)),
    Artifacts = cargo:build_and_capture(C),
    ?assertMatch(2, length(Artifacts)),
    [Main] = [A || A <- Artifacts, cargo_artifact:name(A) =:= <<"main">>],
    [Main2] = [A || A <- Artifacts, cargo_artifact:name(A) =:= <<"main2">>],

    basic_tests(<<"main">>, <<"0.1.0">>, Main),
    basic_tests_bin(<<"main">>, Main),
    basic_tests(<<"main2">>, <<"0.1.0">>, Main2),
    basic_tests_bin(<<"main2">>, Main2),

    ok.


package(Config) ->
    C = cargo:init(?config(crate_path, Config)),
    Artifacts = cargo:build_and_capture(C),
    ?assertMatch([_,_,_], (Artifacts)),
    ct:pal("~p", [Artifacts]),
    [Lib] = [A || A <- Artifacts, cargo_artifact:kind(A) =:= cdylib],
    [Bin] = [A || A <- Artifacts, cargo_artifact:kind(A) =:= bin],
    [Other] = [A || A <- Artifacts, cargo_artifact:kind(A) =:= other],
    basic_tests(<<"lib">>, <<"0.1.0">>, Lib),
    basic_tests_lib(<<"lib">>, Lib),
    basic_tests(<<"bin">>, <<"0.1.0">>, Bin),
    basic_tests_bin(<<"bin">>, Bin),
    basic_tests(<<"other_lib">>, <<"0.1.0">>, Other),

    ok.


init_per_testcase(Name, Config) ->
    DataDir = ?config(data_dir, Config),
    CratePath = filename:join([DataDir, atom_to_list(Name)]),
    [{crate_path, CratePath} | Config].


end_per_testcase(_Name, _Config) ->
    ok.


basic_tests_bin(ExeName, Artifact) ->
    ?assertMatch(bin, cargo_artifact:kind(Artifact)),
    ?assertMatch(
        ExeName,
        filename:rootname(
            filename:basename(cargo_artifact:executable(Artifact))
        )
    ),
    ?assertEqual(
        [cargo_artifact:executable(Artifact)],
        cargo_artifact:filenames(Artifact)
    ).


basic_tests(Name, Version, Artifact) ->
    ?assertMatch(Name, cargo_artifact:name(Artifact)),
    ?assertMatch(Version, cargo_artifact:version(Artifact)),

    [?assertMatch(true, filelib:is_file(N)) || N <- cargo_artifact:filenames(Artifact)],
    ok.


basic_tests_lib(Name, Artifact) ->
    [Lib] = [L || L <- cargo_artifact:filenames(Artifact), cargo_util:is_dylib(L)],
    ?assertNotMatch(nomatch, string:find(Lib, Name)),
    ?assertMatch(cdylib, cargo_artifact:kind(Artifact)).