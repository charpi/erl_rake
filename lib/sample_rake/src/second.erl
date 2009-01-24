-module(second).

-include("../include/foo.hrl").
-include("bar.hrl").

-export([name/0]).

name() ->
    ?MODULE_STRING.
