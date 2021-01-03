%%%-------------------------------------------------------------------
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(lea_server).

-behaviour(gen_server).

%% API
-export([start_link/1, start_link/0, stop/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3, format_status/2]).

-define(SERVER, ?MODULE).

-record(state, {connected = false,
                le_node = undefined}).

%%%===================================================================
%%% API
%%%===================================================================

start_link(Node) ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [Node], []).

start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, ['lesser_evil@touriga'], []).

stop() ->
  gen_server:stop(?SERVER).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([Node]) ->
  process_flag(trap_exit, true),
  erlang:send_after(100, ?SERVER, {connect, Node, 200}),
  {ok, #state{le_node = Node}}.

handle_call(_Request, _From, State) ->
  %% Reply = spawn(fun() -> receive blaa -> ok end end),
  Reply = ok,
  {reply, Reply, State}.

handle_cast(_Request, State) ->
  {noreply, State}.

handle_info({connect, Node, Time}, #state{connected = false} = State) ->
  case net_adm:ping(Node) of
    pong -> {noreply, State#state{connected = true}};
    pang ->
      erlang:send_after(Time, ?SERVER, {connect, Node, Time * 2}),
      {noreply, State}
  end;
handle_info(publish_process_data, #state{connected = true} = State) ->
  PData = lea_process:data(),
  Message = {pdata, node(), benfica},
  Dest = {le_monitor_server, State#state.le_node},
  gen_server:cast(Dest, Message),
  {noreply, State};
handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

format_status(_Opt, Status) ->
  Status.

%%%===================================================================
%%% Internal functions
%%%===================================================================
