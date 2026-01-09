-module(factory_actor_ffi).
-export([actor_new/1, actor_on_message/2, actor_start/1, actor_continue/1, actor_stop/1]).

%% We need to call the REAL gleam@otp@actor from gleam_otp package.
%% Since factory shadows it, we find and call the real module's functions
%% by loading and evaluating the real beam file directly.

-define(CACHED_MOD_KEY, factory_actor_ffi_real_mod).

ensure_real_module_loaded() ->
    case get(?CACHED_MOD_KEY) of
        undefined ->
            %% Force load the real module by manipulating code path
            case find_real_actor_path() of
                {ok, RealPath} ->
                    %% Add real path to front of code path temporarily
                    OldPath = code:get_path(),
                    code:add_patha(RealPath),
                    %% Purge shadow and reload
                    code:purge('gleam@otp@actor'),
                    code:delete('gleam@otp@actor'),
                    code:load_file('gleam@otp@actor'),
                    %% Restore path order (but module stays loaded)
                    code:set_path(OldPath),
                    put(?CACHED_MOD_KEY, loaded),
                    ok;
                error ->
                    put(?CACHED_MOD_KEY, fallback),
                    ok
            end;
        _ -> ok
    end.

find_real_actor_path() ->
    Paths = code:get_path(),
    find_gleam_otp_path(Paths).

find_gleam_otp_path([]) -> error;
find_gleam_otp_path([Path | Rest]) ->
    case string:find(Path, "gleam_otp") of
        nomatch -> find_gleam_otp_path(Rest);
        _ ->
            BeamPath = filename:join(Path, "gleam@otp@actor.beam"),
            case filelib:is_file(BeamPath) of
                true -> {ok, Path};
                false -> find_gleam_otp_path(Rest)
            end
    end.

actor_new(State) ->
    ensure_real_module_loaded(),
    'gleam@otp@actor':new(State).

actor_on_message(Builder, Handler) ->
    'gleam@otp@actor':on_message(Builder, Handler).

actor_start(Builder) ->
    'gleam@otp@actor':start(Builder).

actor_continue(State) ->
    'gleam@otp@actor':continue(State).

actor_stop(Reason) ->
    'gleam@otp@actor':stop(Reason).
