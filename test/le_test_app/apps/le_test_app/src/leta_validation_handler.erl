-module(leta_validation_handler).

-export([init/2]).
-export([terminate/3]).

init(Req, Opts) ->
  Path = cowboy_req:path(Req),
  handle_path(Req, Path, Opts).

handle_path(Req0, _, Opts) ->
%%  {ok, Body, Req1} = cowboy_req:read_body(Req0),
%%  LengthStr = cowboy_req:header(<<"content-length">>, Req1),
%%  Length = binary_to_integer(LengthStr),
%%  erlang:display(Length),
  SizeBin = cowboy_req:binding(size, Req0),
  Size = binary_to_integer(SizeBin),
  HoldList = binary_to_list(crypto:strong_rand_bytes(Size)),
  case Size > 1000 of
    true -> timer:sleep(round(math:pow(2, math:log10(Size))) * 150);
    false -> ok
  end,
  erlang:display(Size),
  Req = cowboy_req:reply(200,
                         #{<<"content-type">> => <<"text/plain">>},
                         integer_to_binary(length(HoldList)),
                         Req0),
  {ok, Req, Opts}.

terminate(_Reason, _Req, _Opts) ->
  ok.
