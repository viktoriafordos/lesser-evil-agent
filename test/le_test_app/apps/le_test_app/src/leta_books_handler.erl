-module(leta_books_handler).

-export([init/2]).
-export([terminate/3]).

init(Req, Opts) ->
  Path = cowboy_req:path(Req),
  handle_path(Req, Path, Opts).

handle_path(Req0, _, Opts) ->
  BookNames = letdb_books:get_names(),
  ResponseBody = jsone:encode(#{books => BookNames}),
  Req = cowboy_req:reply(200,
                         #{<<"content-type">> => <<"application/json">>},
                         ResponseBody,
                         Req0),
  {ok, Req, Opts}.

terminate(_Reason, _Req, _Opts) ->
  ok.
