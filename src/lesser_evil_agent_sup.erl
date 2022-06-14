%%%-------------------------------------------------------------------
%% @doc lesser_evil_agent top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(lesser_evil_agent_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

start_link() ->
    supervisor:start_link(?MODULE, []).

init([]) ->
  SupFlags = #{strategy => one_for_all,
               intensity => 0,
               period => 1},
  ChildSpecs = [#{id => lea_server,
                  start => {lea_server, start_link, []}
                 }],
  {ok, {SupFlags, ChildSpecs}}.