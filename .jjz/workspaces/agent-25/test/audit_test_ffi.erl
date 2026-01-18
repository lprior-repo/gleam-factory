-module(audit_test_ffi).
-export([contains_substring/2]).

%% Check if needle is contained within haystack
contains_substring(Haystack, Needle) ->
    case binary:match(Haystack, Needle) of
        nomatch -> false;
        _ -> true
    end.
