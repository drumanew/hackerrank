-module(solution).
-export([main/0]).

-compile(export_all).

main () ->
  ets:new(?MODULE, [ordered_set, protected, named_table]),
  [_N, Arr, _K, Queries] = %% Input
    parse_input(scan_input()),
  io:format(standard_error, "input parsed~n", []),

  {TimeTree, Tree} =
    timer:tc(fun() -> make_segment_tree(fun factor/1, fun gcm/2, Arr) end),
  io:format(standard_error, "tree built in ~f msec~n", [TimeTree/1000]),

  {Time, Val} =
    timer:tc(
      fun () ->
        serve(Tree, Queries, [])
      end),
  io:format(standard_error, "done in ~f msec~n", [Time/1000]),

  io:format("~s", [Val]),
  ets:delete(?MODULE),
  true.

%-------------------------------------------------------------------------------

scan_input () ->
  Opts = io:getopts(),
  io:setopts([binary]),
  Data = scan_input(<<>>),
  io:setopts(Opts),
  Data.

scan_input (Acc) ->
  case file:read_line(standard_io) of
    {ok, <<"eof\n">>} -> binary_to_list(Acc);
    {ok, Data} -> scan_input(<<Acc/binary, Data/binary>>);
    _ -> binary_to_list(Acc)
  end.

%-------------------------------------------------------------------------------

line_to_int_list (Str) ->
  lists:map(fun list_to_integer/1, string:tokens(Str, " ")).

%-------------------------------------------------------------------------------

parse_input (Str) ->
  Lines = string:tokens(Str, "\n"),
  [N, Arr, K | Rest] = Lines,
  [list_to_integer(N),
   line_to_int_list(Arr),
   list_to_integer(K),
   lists:map(fun (S) ->
               case string:tokens(S, " ") of
                 ["Q", L, R] -> {'Q', list_to_integer(L), list_to_integer(R)};
                 ["U", I, V] -> {'U', list_to_integer(I), list_to_integer(V)}
               end
             end,
             Rest)].

%-------------------------------------------------------------------------------

factor (2) -> orddict:from_list([{2, 1}]);
factor (3) -> orddict:from_list([{3, 1}]);
factor (4) -> orddict:from_list([{2, 2}]);
factor (N) ->
  case catch ets:lookup(?MODULE, N) of
    [{N, PrimesDict}] -> PrimesDict;
    _ ->
      PrimesDict = factor(N, 2, orddict:new()),
      catch ets:insert(?MODULE, {N, PrimesDict}),
      PrimesDict
  end.

factor (1, _, PrimesDict) -> PrimesDict;
factor (N, Divider, PrimesDict) when N rem Divider == 0 ->
  factor(N div Divider, Divider, orddict:update_counter(Divider, 1, PrimesDict));
factor (N, 2, PrimesDict) -> factor(N, 3, PrimesDict);
factor (N, Divider, PrimesDict) -> factor(N, Divider + 2, PrimesDict).

%-------------------------------------------------------------------------------

dict_to_value (D) ->
  orddict:fold(fun (N, Pow, Acc) -> Acc*pow(N, Pow) end, 1, D) rem 1000000007.

%-------------------------------------------------------------------------------

pow (_, 0) -> 1;
pow (N, 1) -> N;
pow (N, Pow) when Pow rem 2 == 0 ->
  Tmp = pow(N, Pow div 2),
  Tmp*Tmp;
pow (N, Pow) -> N*pow(N, Pow-1).

%-------------------------------------------------------------------------------

gcm (A, B) -> gcm([A, B]).

gcm (_V = [H | List]) ->
  lists:foldl(
    fun (Dict, Acc) ->
      orddict:merge(fun (_, A, B) -> max(A, B) end, Dict, Acc)
    end,
    H,
    List).

%-------------------------------------------------------------------------------

serve (_, [], AccOut) -> AccOut;
serve (Tree, [{'Q', L, R} | Queries], AccOut) ->
  % io:format(standard_error, "| Query: (~p, ~p)~n", [L, R]),
  {_Time, Val} =
    timer:tc(
      fun () ->
        dict_to_value(get_value(fun gcm/2, {L, R}, Tree))
      end),
  % io:format(standard_error, "GET query done in ~f msec~n", [_Time/1000]),
  serve(Tree, Queries, [AccOut, [integer_to_list(Val), $\n]]);
serve (Tree, [{'U', I, V} | Queries], AccOut) ->
  % io:format(standard_error, "| Update: (~p, ~p)~n", [I, V]),
  FV = factor(V),
  {_Time, NewTree} =
    timer:tc(
      fun () ->
        update_value(
          fun (Val) -> orddict:merge(fun (_, A, B) -> A + B end, Val, FV) end,
          fun gcm/2,
          I,
          Tree)
      end),
  % io:format(standard_error, "UPD query done in ~f msec~n", [_Time/1000]),
  serve(NewTree, Queries, AccOut).

%-------------------------------------------------------------------------------

-define(node(Int, Val, L, R), {Int, {Val, L, R}}).
-define(int(L, R), {L, R}).

make_segment_tree (InitFun, JoinFun, List) ->
  InitialNodes =
    lists:zip(lists:map(fun (I) -> ?int(I, I) end, lists:seq(0, length(List)-1)),
              lists:map(fun (N) -> {InitFun(N), nil, nil} end, List)),
  join_nodes(JoinFun, InitialNodes).

join_nodes (_, [Tree]) -> Tree;
join_nodes (Fun, Nodes) ->
  join_nodes(Fun, Nodes, []).

join_nodes (Fun, [], Joined) ->
  join_nodes(Fun, Joined);
join_nodes (Fun, [N], Joined) ->
  join_nodes(Fun, Joined ++ [N]);
join_nodes (Fun, [N1, N2 | Rest], Joined) ->
  join_nodes(Fun, Rest, Joined ++ [join_nodes(Fun, N1, N2)]);
join_nodes (Fun,
            N1 = ?node(?int(Min, _), Val1, _, _),
            N2 = ?node(?int(_, Max), Val2, _, _)) when Min =< Max ->
  ?node(?int(Min, Max), Fun(Val1, Val2), N1, N2).

get_value (_, Int, ?node(Int, Val, _, _)) ->
  Val;
get_value (Fun, Int = ?int(_, R),
           ?node(_, _, Left  = ?node(?int(_, MaxL), _, _, _), _)) when R =< MaxL ->
  get_value(Fun, Int, Left);
get_value (Fun, Int = ?int(L, _),
           ?node(_, _, _, Right = ?node(?int(MinR, _), _, _, _))) when MinR =< L ->
  get_value(Fun, Int, Right);
get_value (Fun, ?int(L, R),
           ?node(_, _, Left  = ?node(?int(_, MaxL), _, _, _),
                       Right = ?node(?int(MinR, _), _, _, _))) ->
  Fun(get_value(Fun, ?int(L, MaxL), Left), get_value(Fun, ?int(MinR, R), Right)).

update_value (UpdFun, _, Idx, ?node(Int = ?int(Idx, Idx), Val, Left, Right)) ->
  ?node(Int, UpdFun(Val), Left, Right);
update_value (UpdFun, JoinFun, Idx, ?node(_, _, Left = ?node(?int(_, MaxL), _, _, _), Right)) when Idx =< MaxL ->
  join_nodes(JoinFun, update_value(UpdFun, JoinFun, Idx, Left), Right);
update_value (UpdFun, JoinFun, Idx, ?node(_, _, Left, Right = ?node(?int(MinR, _), _, _, _))) when MinR =< Idx ->
  join_nodes(JoinFun, Left, update_value(UpdFun, JoinFun, Idx, Right)).
