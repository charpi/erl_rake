-module(sample).

-include("../include/sample.hrl").
-include_lib("sample_rake/include/sample_lib.hrl").
-include_lib("common/include/type.hrl").
-include_lib("kernel/include/file.hrl").
-include("bar.hrl").
-include("Data.hrl").

-export([atoms/0]).
-export([breaks_xref/0]).

atoms() -> 
    lists:foldl(fun(X,Acc) -> [X|Acc] end,
	       [],
	       [foobar, bar, baz]).

breaks_xref() ->
    missing:foo().
