%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(leta_books).

-behaviour(gen_server).

%% API
-export([start_link/0, get_names/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2]).

%% temp
-export([initial_state/0]).

-define(SERVER, ?MODULE).
-define(MAX_WORDS, 10000000).
-define(CHAPTERS, 10).

-record(state, {books = []}).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

get_names() ->
  Books = gen_server:call(?SERVER, books),
  [Name || #{book_name := Name} <- Books].

get_watermarked(Id) ->
  ok.

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
  State = initial_state(),
  {ok, State}.

handle_call(books, _From, State) ->
  #state{books = Books} = State,
  {reply, Books, State}.

handle_cast(_Request, State) ->
  {noreply, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

initial_state() ->
  BooksDir = books_dir(),
  Books =
    case file:list_dir(books_dir()) of
      {error, enoent} ->
        ok = file:make_dir(BooksDir),
        create(5, ?MAX_WORDS, BooksDir);
      {ok, BookFileNames} ->
        load(BookFileNames, BooksDir)
    end,
  #state{books = Books}.

books_dir() ->
  {ok, Dir} = file:get_cwd(),
  Dir ++ "/generated_books".

create(0, _, _) -> [];
create(N, MaxWords, BooksDir) ->
  Parent = self(),
  BookWords = MaxWords div N,
  BookName = leta_book_gen:word(12),
  BookPath = mk_path(BooksDir, BookName),
  spawn(fun() ->
            Book = leta_book_gen:book(?CHAPTERS, BookWords div ?CHAPTERS),
            BookCompressed = compress(Book),
            ok = file:write_file(BookPath, BookCompressed),
            Parent ! done
        end),
  Books = [#{book_name => BookName, path => BookPath, id => N}
           | create(N - 1, MaxWords, BooksDir)],
  receive done -> ok end,
  Books.


load(BookFileNames, BooksDir) ->
  [#{book_name => BookName, path => mk_path(BooksDir, BookName)}
   || BookName <- BookFileNames].

mk_path(Path, FileName) ->
  Path ++ "/" ++ FileName.

compress(Binary) ->
  Z = zlib:open(),
  ok = zlib:deflateInit(Z, 9, deflated, 15, 8, huffman_only),
  Compressed = zlib:deflate(Z, Binary),
  Last = zlib:deflate(Z, [], finish),
  ok = zlib:deflateEnd(Z),
  ok = zlib:close(Z),
  list_to_binary([Compressed | Last]).

decompress(Binary) ->
  Z = zlib:open(),
  ok = zlib:inflateInit(Z),
  Decompressed = zlib:inflate(Z, Binary),
  ok = zlib:inflateEnd(Z),
  ok = zlib:close(Z),
  iolist_to_binary(Decompressed).
