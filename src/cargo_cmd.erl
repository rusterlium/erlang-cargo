-module(cargo_cmd).

-export([
    run/2,
    run_with_flags/3
]).

-export_type([
    output/0
]).

-type output() :: list(#{
    binary() => jsx:json_term()
}).

-ifdef(TEST).
-define(MAX_LINE_LENGTH, 32).
-else.
-define(MAX_LINE_LENGTH, 16384).
-endif.

-spec run_with_flags(cargo_opts:t(), iolist(), [iolist()]) -> output().
run_with_flags(Opts, Cmd, Flags) ->

    Inserts =
        lists:foldl(fun({release, true}, Acc) ->
                           ["--release" | Acc];
                       ({target, Target}, Acc) when is_binary(Target) ->
                           [io_lib:format("--target=~s", [Target]) | Acc];
                       ({target_dir, TargetDir}, Acc) when is_binary(TargetDir) ->
                           [io_lib:format("--target-dir=~s", [TargetDir]) | Acc];
                       (_, Acc) ->
                            Acc
                    end,
                    [],
                    [{release, cargo_opts:release(Opts)},
                     {target, cargo_opts:target(Opts)},
                     {target_dir, cargo_opts:target_dir(Opts)}]),

    Cmd1 = [Cmd] ++ Inserts ++ Flags,

    run(Opts, Cmd1).


-spec run(cargo_opts:t(), [iolist()]) -> output().
run(Opts, Args) ->
    C0 = ["cargo"],
    C1 =
    case cargo_opts:toolchain(Opts) of
        undefined ->
            C0;
        Toolchain ->
            C0 ++ [io_lib:format("+~s", [Toolchain])]
    end,

    Cmd = lists:flatten(lists:join(
        " ",
        [binary_to_list(cargo_util:ensure_binary(I)) || I <- C1 ++ Args]
    )),

    exec(Cmd, cargo_opts:path(Opts)).


% Code derived from rebar3
-spec exec(string(), file:filename_all()) -> output().
exec(Command, Path) ->

    Options1 = [
        in,
        binary,
        {cd, Path},
        exit_status,
        {line, ?MAX_LINE_LENGTH},
        use_stdio,
        hide,
        eof,
        {env, env()}
    ],

    Command1 = lists:flatten(patch_on_windows(Command)),
    Port = open_port({spawn, Command1}, Options1),

    try
        case loop(Port, []) of
            {ok, Output} ->
                Output;
            {error, {_Rc, _Output}=Err} ->
                error({cargo_error, Err})
        end
    after
        port_close(Port)
    end.


-spec loop(port(), [T | {incomplete, _}]) -> {ok, [T]} | {error, _}.
loop(Port, Acc) ->
    receive
        {Port, {data, {_, Line}}} ->
            loop(Port, handle_output(Line, Acc));
        {Port, eof} ->
            Data = finalize(Acc),
            receive
                {Port, {exit_status, 0}} ->
                    {ok, Data};
                {Port, {exit_status, Rc}} ->
                    {error, {Rc, Data}}
            end
    end.


-spec patch_on_windows(string()) -> string().
patch_on_windows(Cmd) ->
    case os:type() of
        {win32, nt} ->
            "cmd /q /c " ++ Cmd;
        _ ->
            Cmd
    end.


-spec env() -> [{string(), string()}].
env() ->
    case os:type() of
        {unix, darwin} ->
            % https://github.com/rust-lang/cargo/issues/3287
            % https://github.com/rust-lang/rust/pull/36574
            [{"RUSTFLAGS", "--codegen link-arg=-flat_namespace --codegen link-arg=-undefined --codegen link-arg=suppress"}];
        _ ->
            []
    end.


% Ignore dialyzer warnings for finalize and handle_output as the spec
% for jsx:decode/2 is broken (does not include incomplete and with_tail)
-dialyzer([
    {nowarn_function, finalize/1},
    {nowarn_function, handle_output/2}
]).

finalize([{incomplete, _Decode} | _Acc]) ->
    error(incomplete_json);

finalize(Acc) ->
    lists:reverse(Acc).

handle_output("", Acc) ->
    Acc;

handle_output(<<"">>, Acc) ->
    Acc;

handle_output(Line, Acc0) ->
    {DecodeRes, Acc} =
    case Acc0 of
        [{incomplete, Decode} | Acc1] ->
            {Decode(Line), Acc1};
        Acc2 ->
            {jsx:decode(Line, [return_maps, stream, return_tail]), Acc2}
    end,

    case DecodeRes of
        {with_tail, Res, Tail} ->
            handle_output(Tail, [Res | Acc]);
        Else ->
            [Else | Acc]
    end.