-module(second_tests).

-include_lib("eunit/include/eunit.hrl").
-include("../include/foo.hrl").

name_test() ->
    ?assertEqual("second",second:name()).
