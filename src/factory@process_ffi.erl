-module(factory@process_ffi).
-export([parse_int_safe/1, os_cmd_with_timeout/2]).

parse_int_safe(S) ->
    try
        {ok, list_to_integer(binary_to_list(S))}
    catch
        _:_ -> {error, nil}
    end.

os_cmd_with_timeout(Cmd, TimeoutMs) ->
    Parent = self(),
    Ref = make_ref(),
    Pid = spawn_link(fun() ->
        Result = os:cmd(Cmd),
        Parent ! {Ref, ok, Result}
    end),
    receive
        {Ref, ok, Result} ->
            {ok, Result}
    after TimeoutMs ->
        exit(Pid, kill),
        {error, <<"timeout">>}
    end.
