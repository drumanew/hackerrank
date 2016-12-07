-module(solution).
-export([main/0]).

-compile(export_all).

-record (data, {min = gb_trees:empty(), mid, max = gb_trees:empty(), even}).

% -define(DEBUG).

-ifdef (DEBUG).
-define (DBG (F, A), io:format(standard_error, F, A)).
-else.
-define (DBG (F, A), ok).
-endif.

main () ->
  ets:new(?MODULE, [ordered_set, protected, named_table]),

  [_N, Queries] = %% Input
    parse_input(scan_input()),
    % parse_input(scan_input("in01.txt")),

  ?DBG("input parsed~n", []),

  {_Time, Val} =
    timer:tc(
      fun () ->
        serve(Queries)
      end),
  ?DBG("done in ~f msec~n", [_Time/1000]),

  output(Val),
  % output("out.txt", Val),

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
  [N | Rest] = Lines,
  [list_to_integer(N), lists:map(fun list_to_integer/1, Rest)].

%-------------------------------------------------------------------------------

serve (Queries) ->
  CheckPoints =
    array:foldl(
      fun
        (_, El, Acc) when El > 0 -> Acc;
        (I, El, Acc) -> gb_sets:add(I+El, Acc)
      end,
      gb_sets:new(),
      array:from_list(Queries)),
  ?DBG("checkpoints: ~p~n", [gb_sets:to_list(CheckPoints)]),
  serve(0, #data{}, Queries, CheckPoints, []).

serve (_, _, [], _, Reps) ->
  lists:map(fun (N) -> [integer_to_list(N), "\n"] end, lists:reverse(Reps));
serve (Iter, Data, [Q | Queries], CheckPoints, Reps) when Q >= 0 ->
  NewData = insert(Q, Data),
  M = NewData#data.mid,
  case gb_sets:is_element(Iter, CheckPoints) of
    true ->
      ?DBG("iter#~p: checkpoint: save data~n", [Iter]),
      set_data(Iter, NewData);
    _    -> ok
  end,
  ?DBG("iter#~p: new value: ~p, median: ~p~n", [Iter, Q, M]),
  serve(Iter+1, NewData, Queries, CheckPoints, [M | Reps]);
serve (Iter, _, [Q | Queries], CheckPoints, Reps) when Q < 0 ->
  Data = get_data(Iter+Q),
  M = Data#data.mid,
  case gb_sets:is_element(Iter, CheckPoints) of
    true ->
      ?DBG("iter#~p: checkpoint: save data~n", [Iter]),
      set_data(Iter, Data);
    _    -> ok
  end,
  ?DBG("iter#~p: go to iter#~p, median: ~p~n", [Iter, Iter+Q, M]),
  serve(Iter+1, Data, Queries, CheckPoints, [M | Reps]).

%-------------------------------------------------------------------------------

get_data (Iter) ->
  [{Iter, Data}] = ets:lookup(?MODULE, Iter),
  Data.

%-------------------------------------------------------------------------------

set_data (Iter, Data) ->
  ets:insert(?MODULE, {Iter, Data}).

%-------------------------------------------------------------------------------

insert (N, #data{ mid = undefined }) -> #data{ mid = N, even = false };
insert (N, Data = #data{ min = Min, mid = M, max = Max, even = E }) when N >= M andalso E ->
  {NewMid, NewMax} = take_smallest(insert(N, Max)),
  Data#data{ min = insert(M, Min), mid = NewMid, max = NewMax, even = not E };
insert (N, Data = #data{ mid = M, max = Max, even = E }) when N >= M ->
  Data#data{ max = insert(N, Max), even = not E };
insert (N, Data = #data{ min = Min, even = E }) when E ->
  Data#data{ min = insert(N, Min), even = not E };
insert (N, Data = #data{ min = Min, mid = M, max = Max, even = E }) ->
  {NewMid, NewMin} = take_largest(insert(N, Min)),
  Data#data{ min = NewMin, mid = NewMid, max = insert(M, Max), even = not E };

insert (N, Tree) ->
  case gb_trees:lookup(N, Tree) of
    none -> gb_trees:insert(N, 1, Tree);
    {value, Count} -> gb_trees:update(N, Count+1, Tree)
  end.

take_smallest (Tree) ->
  case gb_trees:smallest(Tree) of
    {N, 1} -> {N, gb_trees:delete(N, Tree)};
    {N, Count} -> {N, gb_trees:update(N, Count-1, Tree)}
  end.

take_largest (Tree) ->
  case gb_trees:largest(Tree) of
    {N, 1} -> {N, gb_trees:delete(N, Tree)};
    {N, Count} -> {N, gb_trees:update(N, Count-1, Tree)}
  end.

to_list (#data{ mid = undefined }) -> nil;
to_list (#data{ min = Min, mid = M, max = Max }) ->
  to_list(Min) ++ [{M}] ++ to_list(Max);
to_list (Tree) ->
  lists:flatten(
    [ lists:duplicate(Count, N) || {N, Count} <- gb_trees:to_list(Tree) ]).
