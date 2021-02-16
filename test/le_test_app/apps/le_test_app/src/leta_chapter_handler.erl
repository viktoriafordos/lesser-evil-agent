-module(leta_chapter_handler).

-export([init/2]).
-export([terminate/3]).

init(Req, Opts) ->
  Path = cowboy_req:path(Req),
  handle_path(Req, Path, Opts).

handle_path(Req0, _, Opts) ->
  BookIdBin = cowboy_req:binding(id, Req0),
  BookId = binary_to_integer(BookIdBin),
  ChapterNumberBin = cowboy_req:binding(chapter_num, Req0),
  ChapterNumber = binary_to_integer(ChapterNumberBin),
  Chapter = letdb_cache:get_content({BookId, ChapterNumber}),
  Req = cowboy_req:reply(200,
                         #{<<"content-type">> => <<"application/octet-stream">>},
                         Chapter,
                         Req0),
  {ok, Req, Opts}.

terminate(_Reason, _Req, _Opts) ->
  ok.
