%%%-------------------------------------------------------------------
%% @doc db cache supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(letdb_cache_sup).

-behaviour(supervisor).

-export([start_link/0, get_cache/1]).

-export([init/1]).

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
  SupFlags = #{strategy => one_for_one,
               intensity => 10,
               period => 5},
  ChildSpecs = [],
  {ok, {SupFlags, ChildSpecs}}.

get_cache(Name) ->
    case [Child || {Id, Child, _, _} <- supervisor:which_children(?SERVER),
                                        Id =:= Name,
                                        is_pid(Child)] of
        [NodeHandlerPid] -> {ok, NodeHandlerPid};
        [] -> add_child(Name)
    end.

add_child(Name) ->
  Res = supervisor:start_child(?SERVER, #{id => Name,
                                          start => {letdb_cache, start_link, [Name]},
                                          type => worker,
                                          restart => temporary
                                         }),
  case Res of
    {ok, Pid} -> {ok, Pid};
    {error, {already_started, Pid}} -> {ok, Pid};
    Other -> Other
  end.
