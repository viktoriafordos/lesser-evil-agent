%%%-------------------------------------------------------------------
%% @doc leta_app public API
%% @end
%%%-------------------------------------------------------------------
-module(leta_app).

-behaviour(application).

-export([start/2, stop/1]).

-define(APP, le_test_app).

start(_StartType, _StartArgs) ->
  start_cowboy(),
  leta_sup:start_link().

stop(_State) ->
  ok = cowboy:stop_listener(http_listener).

%% internal functions
start_cowboy() ->
  {ok, Port} = application:get_env(?APP, http_port),
  {ok, Ip} = application:get_env(?APP, http_ip),
  {ok, ParsedIp} = inet_parse:address(Ip),

  Dispatch =
    cowboy_router:compile(
      [ {'_',
         [ {"/hello-world", leta_hw_handler, []},
           {"/book/:id/chapter/:chapter_num", leta_chapter_handler, []},
           {"/book/:id", leta_book_handler, []}
         ]
        }
      ]),

  {ok, _Pid} = cowboy:start_clear(http_listener,
                                 [{port, Port}, {ip, ParsedIp}],
                                 #{env => #{dispatch => Dispatch}}),
  ok.
