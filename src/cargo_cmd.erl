-module(cargo_cmd).

-export([
    run/2
]).


run(Opts, Args) ->
    C0 = ["cargo"],
    C1 = case cargo_opts:toolchain(Opts) of
        undefined ->
            C0;
        Toolchain ->
            [io_lib:format("+~s", [Toolchain]) | C0]
    end,

    C2 = case cargo_opts:release(Opts) of
        false ->
            C1;
        true ->
            ["--release"|C1]
    end,

    C3 = case cargo_opts:target(Opts) of
        undefined ->
            C2;
        Target ->
            [io_lib:format("--target=~s", Target) | C2]
    end,

    Cmd = lists:flatten(lists:join(
        " ",
        [binary_to_list(cargo_util:ensure_binary(I)) || I <- lists:reverse(C3) ++ Args]
    )),

    exec(Cmd, cargo_opts:path(Opts)).


% Code derived from rebar3
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
            {ok, _Output} = Ok ->
                Ok;
            {error, {_Rc, _Output}=Err} ->
                error({cargo_error, Err})
        end
    after
        port_close(Port)
    end.


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


patch_on_windows(Cmd) ->
    case os:type() of
        {win32, nt} ->
            "cmd /q /c " ++ Cmd;
        _ ->
            Cmd
    end.


env() ->
    case os:type() of
        {unix, darwin} ->
            [{"RUSTFLAGS", "--codegen 'link-args=-flat_namespace -undefined suppress'"}];
        _ ->
            []
    end.