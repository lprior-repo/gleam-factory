-module('factory@resource_ffi').
-export([memory_total/0, memory_processes/0, memory_system/0]).

memory_total() ->
    erlang:memory(total).

memory_processes() ->
    erlang:memory(processes).

memory_system() ->
    erlang:memory(system).
