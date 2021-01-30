%%%-------------------------------------------------------------------
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(lea_server).

-behaviour(gen_server).

%% API
-define(API, [start_link/0]).
-ignore_xref(?API).
-export(?API).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3, format_status/2]).

-define(CONNECT_TIMEOUT, 20000).

-record(state, {connected = false,
                le_node = undefined,
                le_cookie = fun()-> not_a_cookie end,
                report_interval = 10000}).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
  gen_server:start_link(?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
  process_flag(trap_exit, true),

  {ok, ServerHost} = application:get_env(lesser_evil_agent, server_host),
  true = is_atom(ServerHost),
  {ok, ServerCookie} = application:get_env(lesser_evil_agent, server_cookie),
  true = is_atom(ServerCookie),
  {ok, ReportInt} = application:get_env(lesser_evil_agent, report_interval),
  true = (is_integer(ReportInt) andalso ReportInt > 10000),

  self() ! connect,

  {ok, #state{connected = false,
              le_node = ServerHost,
              le_cookie = fun()-> ServerCookie end,
              report_interval = ReportInt}}.

handle_call(_Request, _From, State) ->
  {noreply, State}.

handle_cast(_Request, State) ->
  {noreply, State}.

handle_info(connect,
            #state{connected = false,
                   le_node = Node,
                   le_cookie = CookieFun,
                   report_interval = ReportInt} = State) ->
  true = erlang:set_cookie(Node, CookieFun()),
  case net_adm:ping(Node) of
    pong ->
      true = erlang:monitor_node(Node, true),
      le_cast({new_agent, node(), self()}, State),
      erlang:send_after(ReportInt, self(), publish_process_data),
      {noreply, State#state{connected = true}};
    pang ->
      erlang:send_after(?CONNECT_TIMEOUT, self(), connect),
      {noreply, State}
  end;
handle_info({nodedown, Node}, #state{le_node = Node} = State) ->
  erlang:send_after(?CONNECT_TIMEOUT, self(), connect),
  {noreply, State#state{connected = false}};
handle_info(publish_process_data,
            #state{connected = true,
                   report_interval = ReportInt} = State) ->
  PData = lea_process:data(),
  %SysData = {lea_os:memory_data(), lea_os:cpu_data()},
  SysData = lea_os:memory_data(),
  Message = {report, node(), PData, SysData},
  le_cast(Message, State),
  erlang:send_after(ReportInt, self(), publish_process_data),
  {noreply, State};
handle_info({gc, Pids}, State) ->
  erlang:display({gc, Pids}),
  [erlang:garbage_collect(Pid) || Pid <- Pids],
  {noreply, State};
handle_info({kill, Pids}, State) ->
  erlang:display({kill, Pids}),
  [kill_pid(Pid) || Pid <- Pids],
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

le_cast(Msg, #state{le_node = Node}) ->
  gen_server:cast({le_monitor_server, Node}, Msg).

kill_pid(Pid) ->
  exit(Pid, killed_by_lesser_evil),
  receive
  after
    5000 -> ok
  end,
  case erlang:is_process_alive(Pid) of
    true ->
      exit(Pid, kill),
      receive
      after
        5000 -> ok
      end;
    _ -> ok
  end.
