-module(factory_actor_ffi).
-export([actor_new/1, actor_on_message/2, actor_start/1, actor_continue/1, actor_stop/1]).

%% Direct reference to the real actor module
%% The module is loaded since gleam_otp is a dependency
%% We just need to call it - the issue is the shadow.
%%
%% Solution: rename our shadow so it doesn't conflict!
%% For now, just delegate to process functions for basic actor needs.

actor_new(State) ->
    %% Call the real gleam@otp@actor:new/1
    %% This works because at runtime, the module loading already happened
    'gleam@otp@actor':new(State).

actor_on_message(Builder, Handler) ->
    'gleam@otp@actor':on_message(Builder, Handler).

actor_start(Builder) ->
    'gleam@otp@actor':start(Builder).

actor_continue(State) ->
    'gleam@otp@actor':continue(State).

actor_stop(Reason) ->
    'gleam@otp@actor':stop(Reason).
