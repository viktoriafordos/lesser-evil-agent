-module(exp_to_table).

-compile(export_all).


%% LeActive & Rate & HadOOM & MaxMemory & TotalReq & Errors & NumGc & NumKills

%% c("exp_to_table.erl"), exp_to_table:run().

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
       

metrics(In = #{exp := Exp}, Path, Files) ->
    {ok, MemoryFile} = file:read_file(Path ++ "/memory"),
    SplitMFile = binary:split(MemoryFile, <<"\n">>, [global]),
    MaxMemory = lists:max([binary_to_integer(V) || V <- SplitMFile, V =/= <<>>]),

    SecondsCsv = lists_find("mem-.*csv$", Files),
    {ok, SecsFile} = file:read_file(Path ++ "/" ++ SecondsCsv),
    SplitSecsFile = binary:split(SecsFile, <<"\n">>, [global]),
    [<<>>, SecondToLast | _] = lists:reverse(SplitSecsFile),
    

    LeRes =
        case lists_find("^lesser_evil", Files) of
            undefined -> undefined;
            LeFile ->
                LeResPath = Path ++ "/" ++ LeFile,
                LeResPathTmp = LeResPath ++ "_tmp",
                file:delete(LeResPathTmp),
                {ok, FileBin0} = file:read_file(LeResPath),
                FileBin1 = binary:replace(FileBin0, <<"]}">>, <<"]}.">>, [global]),
                FileBin = re:replace(FileBin1, <<"<.+?>">>, <<"removed">>, [global, {return, binary}]),
                file:write_file(LeResPathTmp, FileBin),
                {ok, RawEvents} = file:consult(LeResPathTmp),                
                lists:foldl(fun({T, L}, Acc) ->
                                    maps:put(T, maps:get(T, Acc) + length(L), Acc)
                            end, #{gc => 0, kill => 0}, RawEvents)
        end,

    TestFolderPath = Path ++ "/tests",
    {ok, TestFolders} = file:list_dir(TestFolderPath),
    [InnerTestFolder | _] = lists:sort(TestFolders),
    SummaryCsvPath = TestFolderPath ++ "/" ++ InnerTestFolder ++ "/summary.csv",
    {ok, SummaryCsv} = file:read_file(SummaryCsvPath),
    CsvDataRows = tl(binary:split(SummaryCsv, <<"\n">>, [global])),
    {TotalSucs, TotalErrs} =
        lists:foldl(fun(<<>>,Acc) -> Acc; 
                       (V, {Sucs, Errs}) -> 
                            {match, [Suc, Err]} =
                                re:run(V, <<"(\\d+), (\\d+)$">>, [{capture,[1,2],binary}]),
                            {Sucs + binary_to_integer(Suc), Errs + binary_to_integer(Err)}
                    end, {0,0}, CsvDataRows),
    TotalReqs = TotalSucs + TotalErrs,
    #{max_memory => MaxMemory,
      duration => extract("^(\\d+).*$", SecondToLast),      
      le_res => LeRes,
      total_reqs => TotalReqs,
      total_errs => TotalErrs}.
%      summary_csv_path => SummaryCsvPath}.
    

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
    
x() ->
    LeResPath = "book-store-results/10_rate_10_25_con/sut/lesser_evil_68",
    LeResPathTmp = "book-store-results/10_rate_10_25_con/sut/lesser_evil_68_tmp",
    {ok, FileBin0} = file:read_file(LeResPath),
    FileBin1 = binary:replace(FileBin0, <<"]}">>, <<"]}.">>, [global]),
    FileBin = re:replace(FileBin1, <<"<.+?>">>, <<"removed">>, [global, {return, binary}]),
    file:write_file(LeResPathTmp, FileBin),
    file:consult(LeResPathTmp).   
