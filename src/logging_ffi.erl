-module(logging_ffi).
-export([secs_to_datetime/1, system_time_ms/0]).

%% Get system time in milliseconds since epoch
system_time_ms() ->
    erlang:system_time(millisecond).

%% Convert epoch seconds to datetime tuple
secs_to_datetime(Secs) ->
    {{Year, Month, Day}, {Hour, Min, Sec}} =
        calendar:gregorian_seconds_to_datetime(Secs + 62167219200),
    {Year, Month, Day, Hour, Min, Sec}.
