-module(sudoku).
-export([
	 solve/1, solve/0
	]).

puzzle_definition() ->
    { { 1, e, e,  e, e, e,  e, e, e },
      { e, e, 2,  7, 4, e,  e, e, e },
      { e, e, e,  5, e, e,  e, e, 4 },

      { e, 3, e,  e, e, e,  e, e, e },
      { 7, 5, e,  e, e, e,  e, e, e },
      { e, e, e,  e, e, 9,  6, e, e },

      { e, 4, e,  e, e, 6,  e, e, e },
      { e, e, e,  e, e, e,  e, 7, 1 },
      { e, e, e,  e, e, 1,  e, 3, e } }.


find_blank_in_row(Row) ->
    find_blank_in_row(Row, 1).

find_blank_in_row(_, 10) ->
    false;
find_blank_in_row(Row, ColNr) when element(ColNr, Row) == e ->
    ColNr;
find_blank_in_row(Row, ColNr) ->
    find_blank_in_row(Row, ColNr + 1).

find_blank(Puzzle) ->
    find_blank(Puzzle, 1).
find_blank(_, 10) ->
    false;
find_blank(Puzzle, RowNr) ->
    ColNr = find_blank_in_row(element(RowNr, Puzzle)),
    if ColNr == false ->
	    find_blank(Puzzle, RowNr + 1);
       true ->
	    {RowNr, ColNr}
    end.

set_element_at({RowNr, ColNr}, Puzzle, Nr) ->
    setelement(RowNr, Puzzle, setelement(ColNr, element(RowNr, Puzzle), Nr)).


%% Verifies that Nr does not appear anywhere in Row.
check_row(Row, Nr) ->
    check_row(Row, Nr, 1).
check_row(_, _, 10) ->
    true;
check_row(Row, Nr, ColNr) ->
    element(ColNr, Row) =/= Nr andalso check_row(Row, Nr, ColNr + 1).

%% Verifies that Nr does not appear anywhere in ColNr
check_column(Puzzle, ColNr, Nr) ->
    check_column(Puzzle, ColNr, Nr, 1).
check_column(_, _, _, 10) ->
    true;
check_column(Puzzle, ColNr, Nr, RowNr) ->
    element(ColNr, element(RowNr, Puzzle)) =/= Nr andalso
	check_column(Puzzle, ColNr, Nr, RowNr + 1).

%% Returns the upper left position of the square containing Pos.
find_square({RowNr, ColNr}) ->
    {((RowNr-1) div 3) * 3 + 1, ((ColNr-1) div 3) * 3 + 1}.

%% Find the square containing Pos and return is as a row.
square_to_row(Puzzle, Pos) ->
    {SqRowNr, SqColNr} = find_square(Pos),
    Row1 = element(SqRowNr, Puzzle),
    Row2 = element(SqRowNr+1, Puzzle),
    Row3 = element(SqRowNr+2, Puzzle),
    { element(SqColNr, Row1),
      element(SqColNr+1, Row1),
      element(SqColNr+2, Row1),
      element(SqColNr, Row2),
      element(SqColNr+1, Row2),
      element(SqColNr+2, Row2),
      element(SqColNr, Row3),
      element(SqColNr+1, Row3),
      element(SqColNr+2, Row3) }.

%% Verifies that the square containing Pos does not contain Nr.
check_square(Puzzle, Pos, Nr) ->
    Row = square_to_row(Puzzle, Pos),
    check_row(Row, Nr).

%% Verify the Puzzle is a correct sudoku puzzle if Nr is placed at {RowNr, ColNr}.
verify_puzzle(Puzzle, {RowNr, ColNr}, Nr) ->
    check_row(element(RowNr, Puzzle), Nr) andalso
	check_column(Puzzle, ColNr, Nr) andalso
	check_square(Puzzle, {RowNr, ColNr}, Nr).

%% Assign a number to Pos, and attempt to solve the resulting puzzle.
%% Returns the solved puzzle, or false if the puzzle could not be solved.
assign(Pos, Puzzle) ->
    assign(Pos, Puzzle, 1).
assign(_, _, 10) ->
    false;
assign(Pos, Puzzle, Nr) ->
    case verify_puzzle(Puzzle, Pos, Nr) of
	false ->
	    assign(Pos, Puzzle, Nr + 1);
	true ->
	    NewPuzzle = set_element_at(Pos, Puzzle, Nr),
	    SolvedPuzzle = solve(NewPuzzle),
	    case SolvedPuzzle of
		false ->
		    assign(Pos, Puzzle, Nr + 1);
		_ ->
		    SolvedPuzzle
	    end
    end.



%% Solve the puzzle. Finds the first blank cell, and calls
%% assign/2. Returns the solved puzzle, or false if the puzzle could
%% not be solved.
solve(Puzzle) ->
    Blank = find_blank(Puzzle),
    case Blank of
	false ->			% if no blank, puzzle is done.
	    Puzzle;
	_ ->
	    assign(Blank, Puzzle)
    end.


solve() ->
    Puzzle = puzzle_definition(),
    {Time, SolvedPuzzle} = timer:tc(sudoku, solve, [Puzzle]),
    Seconds = Time / 1000000,
    io:format("~p~n", [SolvedPuzzle]),
    io:format("Time: ~g~n", [Seconds]).


