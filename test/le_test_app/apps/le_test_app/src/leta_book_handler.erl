-module(leta_book_handler).

-export([init/2]).
-export([terminate/3]).

init(Req, Opts) ->
  Path = cowboy_req:path(Req),
  handle_path(Req, Path, Opts).

handle_path(Req0, _, Opts) ->
  BookIdBin = cowboy_req:binding(id, Req0),
  BookId = binary_to_integer(BookIdBin),
  Name = letdb_book_gen:word(10),
  CompressedBook = letdb_books:get_watermarked(BookId, Name),
  Req = cowboy_req:reply(200,
                         #{<<"content-type">> => <<"application/octet-stream">>},
                         CompressedBook,
                         Req0),
  {ok, Req, Opts}.

terminate(_Reason, _Req, _Opts) ->
  ok.
