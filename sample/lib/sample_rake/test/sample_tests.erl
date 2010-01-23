-module(sample_tests).

-include_lib("eunit/include/eunit.hrl").

atoms_test() ->
    timer:sleep(1000),
    ?assertEqual([baz,bar,foobar],sample:atoms()).
