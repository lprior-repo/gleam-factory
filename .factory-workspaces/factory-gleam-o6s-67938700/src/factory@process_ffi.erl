-module(factory@process_ffi).
-export([parse_int_safe/1]).

parse_int_safe(Bin) ->
    try
        {ok, binary_to_integer(Bin)}
    catch
        _:_ -> {error, nil}
    end.
