-module(workspace_manager_ffi).
-export([cp_reflink/2]).

cp_reflink(Source, Dest) ->
    SrcStr = binary_to_list(Source),
    DestStr = binary_to_list(Dest),
    case file:make_dir(DestStr) of
        ok -> copy_contents(SrcStr, DestStr);
        {error, eexist} -> copy_contents(SrcStr, DestStr);
        {error, Reason} ->
            case filelib:ensure_path(DestStr) of
                ok -> copy_contents(SrcStr, DestStr);
                {error, R} -> {error, list_to_binary(io_lib:format("mkdir ~p failed: ~p / ~p", [DestStr, Reason, R]))}
            end
    end.

copy_contents(Src, Dest) ->
    case file:list_dir(Src) of
        {ok, Files} ->
            lists:foreach(fun(F) ->
                SrcF = Src ++ "/" ++ F,
                DestF = Dest ++ "/" ++ F,
                case filelib:is_dir(SrcF) of
                    true ->
                        _ = file:make_dir(DestF),
                        copy_contents(SrcF, DestF);
                    false ->
                        file:copy(SrcF, DestF)
                end
            end, Files),
            {ok, nil};
        {error, R} -> {error, list_to_binary(io_lib:format("list_dir ~p failed: ~p", [Src, R]))}
    end.
