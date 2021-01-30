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
        maybe_process_info(erlang:process_info(Pid, group_leader),
                           Pid, GroupLeadersApp)
    end,
    erlang:processes()).

maybe_process_info(undefined, _, _) -> false;
%% The group leader of an application should never be killed
maybe_process_info({group_leader, GLPid}, GLPid, _GroupLeadersApp) -> false;
maybe_process_info({group_leader, GLPid}, Pid, GroupLeadersApp) ->
  case maps:find(GLPid, GroupLeadersApp) of
    %% Top level supervisor should never be killed
    {ok, {_App, Pid}} -> false;
    {ok, App} ->
      case erlang:process_info(Pid, [initial_call |?BASE_KEYS]) of
        %% 'Process X' should never be killed
        [{initial_call, {application_master, _, _}} |_] -> false;
        [{initial_call, _} | PI] ->
          case lists:member(GLPid, proplists:get_value(links, PI, [])) of
            true -> false;
            false -> {true, create_proc_data(Pid, App, PI)}
          end;
        _ -> false
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
  maps:filter(fun(_Pid, {App, _TopLevelSup}) ->
                 lists:member(App, Apps)
              end, AppGroupLeaders).

app_group_leaders() ->
  lists:foldl(fun({App, Pid}, Acc) ->
                  case Pid of
                    undefined -> Acc;
                    _ ->
                      {TopLevelSup, _} = application_master:get_child(Pid),
                      Acc#{Pid => {App, TopLevelSup}}
                  end
              end, #{}, proplists:get_value(running,
                                            application_controller:info())).
