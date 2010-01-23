-module(sample_rake_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

start(_Type, _StartArgs) ->
    case sample_rake_sup:start_link() of
	{ok, Pid} -> 
	    {ok, Pid};
	Error ->
	    Error
    end.

stop(_State) ->
    ok.

