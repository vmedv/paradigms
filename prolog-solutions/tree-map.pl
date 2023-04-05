get_size([], 0) :- !.
get_size([X, Y, V, S, Left, Right], S).

update_size([], []) :- !.
update_size([X, Y, V, _, [], []], [X, Y, V, 1, [], []]) :- !.
update_size([X, Y, V, _, [LX, LY, LV, LS, LLeft, LRight], []], 
			[X, Y, V, RS, [LX, LY, LV, LS, LLeft, LRight], []]) :- !, RS is 1 + LS.
update_size([X, Y, V, _, [], [RX, RY, RV, RS, RLeft, RRight]], 
			[X, Y, V, ResS, [], [RX, RY, RV, RS, RLeft, RRight]]) :- !, ResS is 1 + RS.
update_size([X, Y, V, _, [LX, LY, LV, LS, LLeft, LRight], [RX, RY, RV, RS, RLeft, RRight]], 
			[X, Y, V, ResS, [LX, LY, LV, LS, LLeft, LRight], [RX, RY, RV, RS, RLeft, RRight]]) :- ResS is 1 + LS + RS.

split([], _, [], []) :- !.
split([X, Y, V, S, Left, Right], K, RT, Temp2) :- 
		K > X, !, split(Right, K, Temp1, Temp2), update_size([X, Y, V, S, Left, Temp1], RT).
split([X, Y, V, S, Left, Right], K, Temp1, RT) :- 
		K =< X, !, split(Left, K, Temp1, Temp2), update_size([X, Y, V, S, Temp2, Right], RT).

merge([], Tree2, Tree2) :- !.
merge(Tree1, [], Tree1) :- !.
merge([X1, Y1, V1, S1, Left1, Right1], [X2, Y2, V2, S2, Left2, Right2], RT) :- 
		Y1 > Y2, !, merge(Right1, [X2, Y2, V2, S2, Left2, Right2], R), update_size([X1, Y1, V1, S1, Left1, R], RT).
merge([X1, Y1, V1, S1, Left1, Right1], [X2, Y2, V2, S2, Left2, Right2], RT) :- 
		merge([X1, Y1, V1, S1, Left1, Right1], Left2, R), update_size([X2, Y2, V2, S2, R, Right2], RT).

map_get([K, Y, V, S, Left, Right], K, V) :- !.
map_get([X, Y, Val, S, Left, Right], K, V) :- X < K, !, map_get(Right, K, V).
map_get([X, Y, Val, S, Left, Right], K, V) :- X > K, !, map_get(Left, K, V).

change_value([X, Y, V, S, Left, Right], K, NewV, [X, Y, V, S, Left, Res]) :- X < K, !, change_value(Right, K, NewV, Res).
change_value([X, Y, V, S, Left, Right], K, NewV, [X, Y, V, S, Res, Right]) :- X > K, change_value(Left, K, NewV, Res).
change_value([K, Y, V, S, Left, Right], K, NewV, [K, Y, NewV, S, Left, Right]).

map_put(Tree, K, V, Res) :- map_get(Tree, K, _), !, change_value(Tree, K, V, Res).
map_put(Tree, K, V, Res) :- 
		split(Tree, K, Tree1, Tree2), rand_int(1000000000, Y),
		merge(Tree1, [K, Y, V, 1, [], []], TR), merge(TR, Tree2, Res). 

put_several([], Tree, Tree) :- !.
put_several([(K, V) | T], Tree, R) :- map_put(Tree, K, V, R1), put_several(T, R1, R).
map_build([], []) :- !.
map_build([(K, V) | T], Tree) :- rand_int(1000000000, Y), put_several(T, [K, Y, V, 1, [], []], Tree).

kill_left([_, _, _, _, [], Right], Right) :- !.
kill_left([X, Y, V, S, Left, Right], [X, Y, V, S1, Left1, Right]) :- kill_left(Left, Left1), S1 is S - 1.
map_remove(Tree, K, Res) :-
			map_get(Tree, K, _), !, 
			split(Tree, K, Tree1, Tree2), kill_left(Tree2, RTree2), merge(Tree1, RTree2, Res).
map_remove(Tree, _, Tree).

map_num([], K, 0).
map_num([X, _, _, _, Left, Right], K, R) :- X > K, !, map_num(Left, K, R).
map_num([X, _, _, _, Left, Right], K, R) :- X < K, !, map_num(Right, K, R1), get_size(Left, LS), R is R1 + LS + 1.
map_num([K, _, _, _, Left, Right], K, R) :- get_size(Left, LS), R is LS.

map_subMapSize(_, From, To, 0) :- From >= To, !.
map_subMapSize(Tree, From, To, Size) :- map_num(Tree, To, R2), map_num(Tree, From, R1), Size is R2 - R1.