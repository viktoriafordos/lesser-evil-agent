%%%-------------------------------------------------------------------
%% @doc lesser_evil_agent public API
%% @end
%%%-------------------------------------------------------------------

-module(lesser_evil_agent_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    lesser_evil_agent_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
