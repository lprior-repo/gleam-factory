-module(audit_ffi).
-export([get_timestamp/0, get_actor/0]).

%% Get current timestamp in ISO 8601 format with milliseconds
get_timestamp() ->
    {{Year, Month, Day}, {Hour, Min, Sec}} = calendar:universal_time(),
    Ms = erlang:system_time(millisecond) rem 1000,
    iolist_to_binary(io_lib:format("~4..0B-~2..0B-~2..0BT~2..0B:~2..0B:~2..0B.~3..0BZ",
                                   [Year, Month, Day, Hour, Min, Sec, Ms])).

%% Get actor from environment variable or default to system user
get_actor() ->
    case os:getenv("FACTORY_ACTOR") of
        false ->
            case os:getenv("USER") of
                false -> <<"system">>;
                User -> list_to_binary(User)
            end;
        Actor -> list_to_binary(Actor)
    end.
