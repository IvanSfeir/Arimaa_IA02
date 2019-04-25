:- module(bot,
	[ get_moves/3
	]).
% :- use_module(library(apply)).

side(silver).
side(gold).

% détermine si une position est vide ou non
pos_not_empty([Row, Col], Board) :-
	memberchk([Row, Col, _, _], Board).
pos_empty(Coord, Board) :-
	\+pos_not_empty(Coord, Board).


% récupère les coordonnées d'une pièce ou quelle est à telle coordonnée
coord_piece([Row, Col], [Row, Col, Type, Side], Board) :-
	memberchk([Row, Col, Type, Side], Board).


% définit les positions des pièges
pos_is_trap([2, 2]).
pos_is_trap([2, 5]).
pos_is_trap([5, 2]).
pos_is_trap([5, 5]).
% piège + on n'a pas de voisin allié
is_active_trap([Row, Col, _, Side], Board) :-
	pos_is_trap([Row, Col]),
	side(Side),
	\+neighbor([Row, Col], [_, _, _, Side], Board).


% coordonnées voisines de A[row,col]
neighbors_coords([A_row, A_col], [B_row, A_col]) :-
	A_row < 7, B_row is A_row + 1 ;
	A_row > 0, B_row is A_row - 1.
neighbors_coords([A_row, A_col], [A_row, B_col]) :-
	A_col < 7, B_col is A_col + 1 ;
	A_col > 0, B_col is A_col - 1.


% pièces voisines de Pos=[Row,Col]
neighbor(Pos, Neighbor, Board) :-
	neighbors_coords(Pos, NCoord),
	coord_piece(NCoord, Neighbor, Board).
% pièces voisines d'une pièce
neighbor(Piece, Neighbor, Board) :-
	coord_piece(Pos, Piece, Board),
	neighbor(Pos, Neighbor, Board).


% détermine si une pièce est freezée
%is_frozen(Current_piece, Board) :- is_frozen(Current_piece, _).
% détermine si une pièce est freezée + trouve l'ennemi bloquant
is_frozen(Current_piece, Ennemy, Board) :-
	neighbor(Current_piece, Ennemy, Board), % une pièce a coté
	ennemies(Current_piece, Ennemy), % qui est ennemie
	more_powerful(Ennemy, Current_piece), % qui est plus puissant que nous
	[_, _, _, Cur_side] = Current_piece,
	\+neighbor(Current_piece, [_, _, _, Cur_side], Board). % et pas d'allié


% nombre de coups entre 2 positions (au plus court)
nb_moves_to([A_row, A_col], [B_row, B_col], Nb_moves) :-
	Nb_moves is abs(A_row - B_row) + abs(A_col - B_col).


power([_, _, rabbit  , _], 1).
power([_, _, cat     , _], 2).
power([_, _, dog     , _], 3).
power([_, _, horse   , _], 4).
power([_, _, camel   , _], 5).
power([_, _, elephant, _], 6).
more_powerful(A, B) :-
	power(A, PA), power(B, PB), PA > PB.


allies([_, _, _, Side], [_, _, _, Side]).
ennemies(A, B) :- \+allies(A, B).


% try all the possible moves
%% move(Piece, MoveData, _, Board) :-
%%	[_, _, rabbit, _] = Piece, !,
%%	\+is_frozen(Piece, _, Board),
%%	move_simple(Piece, MoveData, Board).
move(Piece, MoveData, NbMoves, Board) :-
	\+is_frozen(Piece, _, Board),
	(	move_simple(Piece, MoveData, Board) ;
	 	NbMoves > 1, move_push(Piece, MoveData, Board) ;
	 	NbMoves > 1, move_pull(Piece, MoveData, Board)).
	% TODO avoid freeze


% move_simple(Piece, MoveData, Board)
move_simple(Piece, [[Pos, NewPos]], Board) :-  % cas spécial pour les lapins
	[_, _, rabbit, Side] = Piece, !,
	coord_piece(Pos, Piece, Board),
	neighbors_coords(Pos, NewPos),
	[Pr, _] = Pos, [Nr, _] = NewPos,
	(Side = gold, Nr < Pr ; Side = silver, Nr > Pr), % on peut pas reculer
	pos_empty(NewPos, Board).
move_simple(Piece, [[Pos, NewPos]], Board) :- % cas normalement
	coord_piece(Pos, Piece, Board),
	neighbors_coords(Pos, NewPos),
	pos_empty(NewPos, Board).

% move_push(Piece, MoveData, Board)
move_push(Piece, [[Npos, NNewPos], [Pos, Npos]], Board) :-
	neighbor(Piece, N, Board),
	[_, _, _, PSide] = Piece, [_, _, _, NSide] = N, PSide \= NSide,
	more_powerful(Piece, N),
	coord_piece(N, Npos, Board),
	neighbors_coords(Npos, NNewPos),
	pos_empty(NNewPos, Board),
	coord_piece(Pos, Piece, Board).

% move_pull(Piece, MoveData, Board)
move_pull(Piece, [[Pos, NewPos], [NPos, Pos]], Board) :-
	neighbor(Piece, N, Board),
	[_, _, _, PSide] = Piece, [_, _, _, NSide] = N, PSide \= NSide,
	more_powerful(Piece, N),
	coord_piece(Pos, Piece, Board),
	neighbors_coords(Pos, NewPos),
	pos_empty(NewPos, Board),
	coord_piece(NPos, N, Board).


simulate_move(Board, [], Board).
simulate_move(OldBoard, [[[From_row, From_col], [To_row, To_col]] |MoveData], NewBoard) :-
	selectchk([From_row, From_col, Type, Side], OldBoard, [To_row, To_col, Type, Side], TempBoard), % on remplace la piece
	simulate_trap(TempBoard, TrapTempBoard), % enleve la piece si elle est dans une trap
	simulate_move(TrapTempBoard, MoveData, NewBoard), !.

simulate_trap(Board, NewBoard) :-
	is_active_trap(PieceTrapped, Board), % piege sans voisin allié
	selectchk(PieceTrapped, Board, NewBoard), !. % on enleve la piece
simulate_trap(SameBoard, SameBoard). % pas de piege ici


do_turn(Board, Side, MD, NextBoard) :-
	do_turn_impl(Board, Side, MD_, 4, NextBoard), append(MD_, MD).

do_turn_impl(Board, Side, [MD|MDs], NbMovesRemaining, NextBoard) :-
	NbMovesRemaining > 0,
	member(Piece, Board), % on choisit une piece
	[_, _, _, Side] = Piece, % du bon côté
	move(Piece, MD, NbMovesRemaining, Board), % on génère un mouvement possible de cette pièce
	simulate_move(Board, MD, NewBoard), % on simule le board résultant du mouvement
	length(MD, MoveCost),
	NbNextMove is NbMovesRemaining - MoveCost, % on calcule le nb de coups restants dans ce tour
	( MDs = [], NextBoard = NewBoard ; do_turn_impl(NewBoard, Side, MDs, NbNextMove, NextBoard)).

% sum silver pow - sum gold pow + (7 - min dist silver rabbit)^2
heuristic(Board, Score) :-
	maplist(heuristic_piece, Board, PieceScores),
	
	include(is_silver_rabbit, Board, Rabbits),
	maplist(dist_rabbit, Rabbits, DistRabbits),
	list_min(DistRabbits, DistMinRabbits),
	
	sum_list(PieceScores, ScoreInt),
	
	Score is ScoreInt + (7 - DistMinRabbits)*(7 - DistMinRabbits).
	
is_silver_rabbit([_, _, rabbit, silver]).

heuristic_piece(Piece, Score) :-
	power(Piece, Pow),
	[_, _, _, Side] = Piece,
	( Side = silver ->
		SideMul is 1
	; Side = gold ->
		SideMul is -1),
	%% ( is_frozen(Piece, _, Board) ->
	%%	Frozen_score is -1
	%% ; Frozen_score is 0),
    Score is SideMul*Pow.

dist_rabbit([Row, _Col, rabbit, Side], Dist) :-
	( Side = silver ->
		Dist is 7 - Row
	; Side = gold ->
		Dist is Row)
	, !.
dist_rabbit(_, 0).

list_min([L|Ls], Min) :-
    list_min(Ls, L, Min).

list_min([], Min, Min).
list_min([L|Ls], Min0, Min) :-
    Min1 is min(L, Min0),
    list_min(Ls, Min1, Min).


% for debugging
print_piece(rabbit  , silver) :- write('a ').
print_piece(cat     , silver) :- write('b ').
print_piece(dog     , silver) :- write('c ').
print_piece(horse   , silver) :- write('d ').
print_piece(camel   , silver) :- write('e ').
print_piece(elephant, silver) :- write('f ').
print_piece(rabbit  , gold) :- write('1 ').
print_piece(cat     , gold) :- write('2 ').
print_piece(dog     , gold) :- write('3 ').
print_piece(horse   , gold) :- write('4 ').
print_piece(camel   , gold) :- write('5 ').
print_piece(elephant, gold) :- write('6 ').

print_coord(Row, Col, Board) :-
	memberchk([Row, Col, Type, Side], Board),
	print_piece(Type, Side), !.
print_coord(_, _, _) :-
	write('. '), !.

print_board(Board) :-
	print_board(7, 0, Board).
print_board(0, 7, Board) :-
	print_coord(0, 7, Board),
	writeln(''), !.
print_board(Row, 7, Board) :-
	print_coord(Row, 7, Board),
	writeln(''),
	NRow is Row-1,
	print_board(NRow, 0, Board), !.
print_board(Row, Col, Board) :-
	print_coord(Row, Col, Board),
	NCol is Col+1,
	print_board(Row, NCol, Board).


test(move, Moves) :-
	example_board(Board),
	time(findall([P, MD], (member(P, Board), move(P, MD, 4, Board)), Moves)).


% get_moves signature : get_moves(Moves, gamestate, board).

% Exemple of variable
% gamestate: [side, [captured pieces] ] (e.g. [silver, [ [0, 1, rabbit, silver], [0, 2, horse, silver] ])

% use this for debugging
example_board(Board) :-
	Board = [
		[0, 0, rabbit,   silver],
		[0, 1, rabbit,   silver],
		[0, 2, horse,    silver],
		[0, 3, rabbit,   silver],
		[0, 4, elephant, silver],
		[0, 5, rabbit,   silver],
		[0, 6, rabbit,   silver],
		[0, 7, rabbit,   silver],
		[1, 0, camel,    silver],
		[1, 1, cat,      silver],
		[1, 2, rabbit,   silver],
		[1, 3, dog,      silver],
		[1, 4, rabbit,   silver],
		[1, 5, horse,    silver],
		[1, 6, dog,      silver],
		[1, 7, cat,      silver],
		[6, 7, rabbit,   gold],
		[6, 0, cat,      gold],
		[6, 1, horse,    gold],
		[6, 2, camel,    gold],
		[6, 3, elephant, gold],
		[6, 4, rabbit,   gold],
		[6, 5, dog,      gold],
		[6, 6, rabbit,   gold],
		[7, 0, rabbit,   gold],
		[7, 1, rabbit,   gold],
		[7, 2, rabbit,   gold],
		[7, 3, cat,      gold],
		[7, 4, dog,      gold],
		[7, 5, rabbit,   gold],
		[7, 6, horse,    gold],
		[7, 7, rabbit,   gold]
	].

% Call exemple get_moves(Moves, [silver, []], Example_board).
% default call get_moves([[[1, 0], [2, 0]], [[0, 0], [1, 0]], [[0, 1], [0, 0]], [[0, 0], [0, 1]]], Gamestate, Board).

get_move().

% Gamestate: [side, [captured pieces]]
% Board: [[row, col, piece_type, side], [row, col, piece_type, side], ...]
% Moves : [[[old_row, old_col], [new_row, new_col]], [...], [...], [...]]
get_moves(FinalMove, Gamestate, Board) :-
	findall([Score, MD], (do_turn(Board, silver, MD, NB), heuristic(NB, Score)), Moves),
	sort(1, @>=, Moves, SortedMoves),
	[FinalMove|_] = SortedMoves.
