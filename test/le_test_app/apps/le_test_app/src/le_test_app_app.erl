%%%-------------------------------------------------------------------
%% @doc le_test_app public API
%% @end
%%%-------------------------------------------------------------------

-module(le_test_app_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    le_test_app_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
