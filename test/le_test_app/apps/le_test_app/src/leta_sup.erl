%%%-------------------------------------------------------------------
%% @doc le_test_app top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(leta_sup).

-behaviour(supervisor).

-export([start_link/0, init/1]).

-define(SERVER, ?MODULE).

start_link() ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%% sup_flags() = #{strategy => strategy(),         % optional
%%                 intensity => non_neg_integer(), % optional
%%                 period => pos_integer()}        % optional
%% child_spec() = #{id => child_id(),       % mandatory
%%                  start => mfargs(),      % mandatory
%%                  restart => restart(),   % optional
%%                  shutdown => shutdown(), % optional
%%                  type => worker(),       % optional
%%                  modules => modules()}   % optional
init([]) ->
  spawn(fun logger/0),
  SupFlags = #{strategy => one_for_all,
               intensity => 0,
               period => 1},
  ChildSpecs = [],
  {ok, {SupFlags, ChildSpecs}}.

logger() ->
    File = "/measurements/memory",
    {ok, Fd} = file:open(File, [append]),
    logger_loop(Fd).

logger_loop(Fd) ->
    Mem = round(erlang:memory(total)/1024/1024),
    Log = io_lib:format("~p~n", [Mem]),
    file:write(Fd, Log),
    receive
    after
        1000 -> logger_loop(Fd)
    end.

