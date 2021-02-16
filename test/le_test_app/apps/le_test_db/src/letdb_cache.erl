%%%-------------------------------------------------------------------
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(letdb_cache).

-behaviour(gen_server).

%% API
-export([start_link/1, get_content/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API
%%%===================================================================
start_link(Id) ->
  gen_server:start_link(?MODULE, [Id], []).

get_content(Id) ->
  try
    {ok, Pid} = letdb_cache_sup:get_cache(Id),
    gen_server:call(Pid, get)
  catch
    _:_:_ -> {error, not_found}
  end.


%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
init([{Id, ChapterId}]) ->
  case letdb_books:get_chapter(Id, ChapterId) of
    {error, not_found} -> ignore;
    Chapter -> {ok, Chapter}
  end;
init([Id]) ->
  case letdb_books:get_book(Id) of
    {error, not_found} -> ignore;
    Book -> {ok, Book}
  end.
handle_call(get, _From, State) ->
  {reply, State, State}.

handle_cast(_Request, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.


%%%===================================================================
%%% Internal functions
%%%===================================================================
