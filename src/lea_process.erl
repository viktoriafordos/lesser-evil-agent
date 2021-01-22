%% module to extract process info data
-module(lea_process).

-define(BASE_KEYS, [memory, message_queue_len, priority, links, monitored_by, reductions]).
-define(KEYS, [group_leader | ?BASE_KEYS]).

-define(API, [data/0, data_m/0, data/1,
              app_group_leaders/1, filtered_app_group_leaders/1]).
-ignore_xref(?API).
-export(?API).

data(Apps) ->
  GroupLeadersApp = filtered_app_group_leaders(Apps),
  lists:filtermap(
    fun(Pid) ->
        maybe_process_info(erlang:process_info(Pid, group_leader), Pid, GroupLeadersApp)
    end,
    erlang:processes()).

maybe_process_info(undefined, _, _) -> false;
maybe_process_info({_, GLPid}, Pid, GroupLeadersApp) ->
  case maps:find(GLPid, GroupLeadersApp) of
    {ok, App} ->
      case erlang:process_info(Pid, ?BASE_KEYS) of
        undefined -> false;
        PI -> {true, [{application, App} | PI]}
      end;
    error ->
      false
  end.

data() ->
  AppGroupLeaders = app_group_leaders(pid_app),
  lists:filtermap(
    fun(Pid) ->
        case erlang:process_info(Pid, ?KEYS) of
          undefined -> false;
          PI -> {true, {Pid, add_app(PI, AppGroupLeaders)}}
        end
    end,
    erlang:processes()).

data_m() ->
  AppGroupLeaders = app_group_leaders(pid_app),
  [begin
     PI = maps:from_list(erlang:process_info(Pid, ?KEYS)),
     {Pid, add_app_m(PI, AppGroupLeaders)}
   end
   || Pid <- erlang:processes()].

add_app(PI, GL) ->
  %% Pid = proplists:get_value(group_leader, PI),
  %% lists:keyreplace(group_leader, 1, PI, {application, maps:get(Pid, GL, undefined)}).
  lists:foldl(fun({group_leader, Pid}, Acc) ->
                  [{application, maps:get(Pid, GL, undefined)} | Acc];
                 (V, Acc) -> [V | Acc]
              end, [], PI).

add_app_m(PI0, GL) ->
  {Pid, PI1} = maps:take(group_leader, PI0),
  PI1#{application => maps:get(Pid, GL, undefined)}.

filtered_app_group_leaders(Apps) ->
  AppGroupLeaders = app_group_leaders(pid_app),
  maps:filter(fun(_Pid, App) -> lists:member(App, Apps) end, AppGroupLeaders).

app_group_leaders(Way) ->
  WayFun = way_fun(Way),
  lists:foldl(fun({App, Pid}, Acc) ->
                  case Pid of
                    undefined -> Acc;
                    _ -> WayFun(Pid, App, Acc)
                  end
              end, #{}, proplists:get_value(running, application_controller:info())).

way_fun(pid_app) -> fun pid_app/3;
way_fun(app_pid) -> fun app_pid/3.

pid_app(Pid, App, Acc) -> Acc#{Pid => App}.
app_pid(Pid, App, Acc) -> Acc#{App => Pid}.
%% (lesser_evil_agent@touriga)39> timer:tc(fun() -> lists:foreach(fun(_) -> lea_process:data() end, lists:seq(1,100)) end).
%% {92241,ok}
%% (lesser_evil_agent@touriga)40> timer:tc(fun() -> lists:foreach(fun(_) -> lea_process:data_m() end, lists:seq(1,100)) end).
%% {84423,ok}








%% 8> erlang:process_info(self(), binary).                                                                                                                                                                          {binary,[{339769704,281,2},{339769376,125,2}]}
%% 9> erlang:process_info(self(), memory).
%% {memory,88532}
%% 10> erlang:process_info(self(), message_queue_len).
%% {message_queue_len,0}
%% 11> erlang:process_info(self(), heap_size).
%% {heap_size,6772}
%% 12> erlang:process_info(self(), total_heap_size).
%% {total_heap_size,13544}
%% 13> erlang:process_info(self(), garbage_collection).
%% {garbage_collection,[{max_heap_size,#{error_logger => true,kill => true,size => 0}},
%%                      {min_bin_vheap_size,46422},
%%                      {min_heap_size,233},
%%                      {fullsweep_after,65535},
%%                      {minor_gcs,2}]}
%% 14> erlang:process_info(self()).
%% [{current_function,{erl_eval,do_apply,6}},
%%  {initial_call,{erlang,apply,2}},
%%  {status,running},
%%  {message_queue_len,0},
%%  {links,[<0.130.0>]},
%%  {dictionary,[]},
%%  {trap_exit,false},
%%  {error_handler,error_handler},
%%  {priority,normal},
%%  {group_leader,<0.129.0>},
%%  {total_heap_size,13544},
%%  {heap_size,6772},
%%  {stack_size,24},
%%  {reductions,20186},
%%  {garbage_collection,[{max_heap_size,#{error_logger => true,kill => true,size => 0}},
%%                       {min_bin_vheap_size,46422},
%%                       {min_heap_size,233},
%%                       {fullsweep_after,65535},
%%                       {minor_gcs,3}]},
%%  {suspending,[]}]


%%erlang:process_info(<0.10.0>, [memory, message_queue_len, heap_size, total_heap_size, garbage_collection]).
