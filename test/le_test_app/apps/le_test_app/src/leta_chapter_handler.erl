-module(leta_chapter_handler).

-export([init/2]).
-export([terminate/3]).

init(Req, Opts) ->
  Path = cowboy_req:path(Req),
  handle_path(Req, Path, Opts).

handle_path(Req0, _, Opts) ->
  BookId = cowboy_req:binding(id, Req0),
  ChapterNumber = cowboy_req:binding(chapter_number, Req0),
  Req = cowboy_req:reply(200,
                         #{<<"content-type">> => <<"text/plain">>},
                         <<"Viva ao Benfica!">>,
                         Req0),
  {ok, Req, Opts}.

terminate(_Reason, _Req, _Opts) ->
  ok.
