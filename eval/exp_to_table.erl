-module(exp_to_table).

-compile(export_all).


%% LeActive & Rate & HadOOM & MaxMemory & TotalReq & Errors & NumGc & NumKills

run() ->
    ED = #{ exp => "embedded-device-results",
            active_folder => "sut-ed",
            inactive_folder => "sut-wo"
          },
    BK = #{ exp => "book-store-results",
            active_folder => "sut",
            inactive_folder => "sut-wo"
          },
    EDRes = results(ED),
    BKRes = results(BK),
    io:format("~p~n~n~n", [EDRes]),
    io:format("~p~n", [BKRes]).
    
results(In = #{exp := Exp}) ->
    {ok, Runs} = file:list_dir(Exp),
    lists:map(
      fun(RunName) ->
              RateStr = extract(".*_(\\d+)_\\d+_con$", RunName),
              run_results(In#{rate => list_to_integer(RateStr),
                              run_path => Exp ++ "/" ++ RunName})
      end, Runs).

run_results(In = #{run_path := RunPath,
                   active_folder := ActF,
                   inactive_folder := InactF
                  }) ->
    ActPath = RunPath ++ "/" ++ ActF,
    InactPath = RunPath ++ "/" ++ InactF,
    {ok, ActFiles} = file:list_dir(ActPath),
    {ok, InactFiles} = file:list_dir(InactPath),
    In#{active_data => metrics(In, ActPath, lists:sort(ActFiles)),
       inactive_data => metrics(In, InactPath, lists:sort(InactFiles))}.
       

metrics(In, Path, Files) ->
    {ok, MemoryFile} = file:read_file(Path ++ "/memory"),
    SplitMFile = binary:split(MemoryFile, <<"\n">>, [global]),
    MaxMemory = lists:max([binary_to_integer(V) || V <- SplitMFile, V =/= <<>>]),

    SecondsCsv = lists_find("mem-.*csv$", Files),
    {ok, SecsFile} = file:read_file(Path ++ "/" ++ SecondsCsv),
    SplitSecsFile = binary:split(SecsFile, <<"\n">>, [global]),
    [<<>>, SecondToLast | _] = lists:reverse(SplitSecsFile),
    

    LeRes = lists_find("^lesser_evil", Files),
    #{max_memory => MaxMemory,
      duration => extract("^(\\d+).*$", SecondToLast),
      sec_last => SecondToLast,
      le_res => LeRes}.
    

lists_find(Pattern, []) -> undefined;
lists_find(Pattern, [H|T]) ->
    case re:run(H, Pattern) of               
        {match, _} -> H;
        _  -> lists_find(Pattern, T)
    end.

extract(Pattern, V) ->
    {match, [Res]} =
        re:run(V, Pattern, [{capture,[1],list}]),
    Res.
    
