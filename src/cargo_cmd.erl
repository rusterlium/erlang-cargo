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


-spec run_with_flags(cargo_opts:t(), iolist(), [iolist()]) -> output().
run_with_flags(Opts, Cmd, Flags) ->
    Insert0 =
    case cargo_opts:release(Opts) of
        true ->
            ["--release"];
        _ ->
            []
    end,

    Insert1 =
    case cargo_opts:target(Opts) of
        undefined ->
            Insert0;
        Target ->
            [io_lib:format("--target=~s", [Target]) | Insert0]
    end,

    Cmd1 = [Cmd] ++ Insert1 ++ Flags,

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
    OutputHandler =
    fun (Line, Acc) when Line =/= "" ->
            [jsx:decode(Line, [return_maps]) | Acc];
        (_, Acc) ->
            Acc
    end,

    Options1 = [
        in,
        binary,
        {cd, Path},
        exit_status,
        {line, 16384},
        use_stdio,
        hide,
        eof,
        {env, env()}
    ],

    Command1 = lists:flatten(patch_on_windows(Command)),
    Port = open_port({spawn, Command1}, Options1),

    try
        case loop(Port, OutputHandler, []) of
            {ok, Output} ->
                Output;
            {error, {_Rc, _Output}=Err} ->
                error({cargo_error, Err})
        end
    after
        port_close(Port)
    end.


-spec loop(port(), fun((ok, [T]) -> T), [T]) -> {ok, [T]} | {error, _}.
loop(Port, Fun, Acc) ->
    receive
        {Port, {data, {_, Line}}} ->
            loop(Port, Fun, Fun(Line, Acc));
        {Port, eof} ->
            Data = lists:reverse(Acc),
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
            [{"RUSTFLAGS", "--codegen 'link-args=-flat_namespace -undefined suppress'"}];
        _ ->
            []
    end.