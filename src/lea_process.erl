%% module to extract process info data
-module(lea_process).

-define(BASE_KEYS, [memory, message_queue_len, priority, links, monitored_by, reductions]).
-define(KEYS, [group_leader | ?BASE_KEYS]).

-define(API, [data/0, data/1,
              app_group_leaders/0, filtered_app_group_leaders/1]).
-ignore_xref(?API).
-export(?API).

data() ->
  {ok, Apps} = application:get_env(lesser_evil_agent, applications),
  data(Apps).

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
        PI -> {true, create_proc_data(Pid, App, PI)}
      end;
    error ->
      false
  end.

create_proc_data(Pid, App, PI) ->
  M = maps:from_list(PI),
  M#{pid => Pid,
     application => App,
     monitored_by => length(maps:get(monitored_by, M)),
     links => length(maps:get(links, M))}.

filtered_app_group_leaders(Apps) ->
  AppGroupLeaders = app_group_leaders(),
  maps:filter(fun(_Pid, App) -> lists:member(App, Apps) end, AppGroupLeaders).

app_group_leaders() ->
  lists:foldl(fun({App, Pid}, Acc) ->
                  case Pid of
                    undefined -> Acc;
                    _ -> Acc#{Pid => App}
                  end
              end, #{}, proplists:get_value(running, application_controller:info())).
