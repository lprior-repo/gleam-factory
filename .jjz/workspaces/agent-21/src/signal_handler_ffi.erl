-module(signal_handler_ffi).
-behaviour(gen_event).

-export([setup_signal_handlers/1, remove_signal_handlers/0]).
-export([init/1, handle_event/2, handle_call/2, handle_info/2, terminate/2]).

%% Setup signal handlers - registers this module with erl_signal_server
setup_signal_handlers(Callback) ->
    %% Remove default handler first
    case gen_event:delete_handler(erl_signal_server, erl_signal_handler, []) of
        ok -> ok;
        {error, module_not_found} -> ok
    end,
    %% Add our custom handler
    gen_event:add_handler(erl_signal_server, ?MODULE, Callback).

%% Remove signal handlers - restores default behavior
remove_signal_handlers() ->
    case gen_event:delete_handler(erl_signal_server, ?MODULE, []) of
        ok -> ok;
        {error, module_not_found} -> ok
    end,
    gen_event:add_handler(erl_signal_server, erl_signal_handler, []).

%% gen_event callbacks
init(Callback) ->
    {ok, #{callback => Callback}}.

handle_event(sigterm, #{callback := Callback} = State) ->
    Callback(sigterm),
    {ok, State};
handle_event(sigint, #{callback := Callback} = State) ->
    %% For SIGINT (Ctrl+C), also trigger shutdown
    Callback(sigint),
    {ok, State};
handle_event(sigusr1, State) ->
    %% Ignore USR1
    {ok, State};
handle_event(sigusr2, State) ->
    %% Ignore USR2
    {ok, State};
handle_event(_Signal, State) ->
    {ok, State}.

handle_call(_Request, State) ->
    {ok, ok, State}.

handle_info(_Info, State) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.
