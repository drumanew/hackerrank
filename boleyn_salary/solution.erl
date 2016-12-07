-module(solution).
-export([main/0]).

-compile(export_all).

-define(DEBUG, ok).

-ifdef (DEBUG).
-define (DBG (F, A), io:format(standard_error, F, A)).
-else.
-define (DBG (F, A), ok).
-endif.

main () ->
  ets:new(?MODULE, [ordered_set, protected, named_table]),

  [_, Links, Salary, Queries] = %% Input
    % parse_input(scan_input()),
    parse_input(scan_input("in10.txt")),

  ?DBG("input parsed~n", []),

  {_Time, Val} =
    timer:tc(
      fun () ->
        serve(Links, Salary, Queries)
      end),
  ?DBG("done in ~f msec~n", [_Time/1000]),

  % output(Val),
  output("out.txt", Val),

  ets:delete(?MODULE),
  true.

%-------------------------------------------------------------------------------

scan_input () ->
  Opts = io:getopts(),
  io:setopts([binary]),
  Data = scan_input(standard_io, <<>>),
  io:setopts(Opts),
  Data.

scan_input (FileName) ->
  ?DBG("scan input test file: ~s~n", [FileName]),
  {ok, File} = file:open(FileName, [read, binary]),
  Data = scan_input(File, <<>>),
  file:close(File),
  Data.

scan_input (IODev, Acc) ->
  case file:read_line(IODev) of
    {ok, <<"eof\n">>} -> binary_to_list(Acc);
    {ok, Data} -> scan_input(IODev, <<Acc/binary, Data/binary>>);
    _ -> binary_to_list(Acc)
  end.

%-------------------------------------------------------------------------------

output (Val) ->
  io:format("~s", [Val]).

output (FileName, Val) ->
  {ok, File} = file:open(FileName, [write]),
  io:format(File, "~s", [Val]),
  file:close(File).

%-------------------------------------------------------------------------------

line_to_int_list (Str) ->
  lists:map(fun list_to_integer/1, string:tokens(Str, " ")).

%-------------------------------------------------------------------------------

parse_input (Str) ->
  Lines = string:tokens(Str, "\n"),
  [NQ | LinksSalaryQueries] = Lines,
  [N, Q] = line_to_int_list(NQ),
  LinksStrings = lists:sublist(LinksSalaryQueries, N-1),
  SalaryQueries = LinksSalaryQueries -- LinksStrings,
  Links = [ list_to_tuple(line_to_int_list(S)) || S <- LinksStrings ],
  [SalaryStr | QueriesStrings] = SalaryQueries,
  Salary = line_to_int_list(SalaryStr),
  Queries = [ list_to_tuple(line_to_int_list(S)) || S <- QueriesStrings ],
  [[N, Q], Links, Salary, Queries].

%-------------------------------------------------------------------------------

serve (Links, Salary, Queries) ->
  G = digraph:new(),
  lists:foldl(fun (S, I) -> digraph:add_vertex(G, I, S), I+1 end, 1, Salary),
  lists:foreach(fun ({C, P}) -> digraph:add_edge(G, P, C) end, Links),
  {_, Reps} =
    lists:foldl(
      fun ({V, K}, {D, Reps}) ->
        ?DBG("Query: V:~p, K:~p, D:~p~n", [V, K, D]),
        SubordinatesSalary = get_subs_salary(G, V+D),
        {NewD, _} = lists:nth(K, SubordinatesSalary),
        ?DBG("Reply: ~p~n", [NewD]),
        {NewD, [NewD | Reps]}
      end,
      {0, []},
      Queries),
  digraph:delete(G),
  lists:map(fun (N) -> [integer_to_list(N), "\n"] end, lists:reverse(Reps)).

get_subs_salary (G, V) ->
  case ets:lookup(?MODULE, V) of
    [{V, SubordinatesSalary}] -> SubordinatesSalary;
    _ ->
      SubordinatesSalary =
        lists:sort(
          fun ({_, S1}, {_, S2}) -> S1 < S2 end,
          [ begin
              {V, S} = digraph:vertex(G, V),
              {S, V}
            end || V <- digraph_utils:reachable_neighbours([V], G) ]),
      ets:insert(?MODULE, {V, SubordinatesSalary}),
      SubordinatesSalary
  end.
