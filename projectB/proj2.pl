% Load the clpfd library
:- ensure_loaded(library(clpfd)).

% Given Code

main(PuzzleFile, WordlistFile, SolutionFile) :-
	read_file(PuzzleFile, Puzzle),
	read_file(WordlistFile, Wordlist),
	valid_puzzle(Puzzle),
	solve_puzzle(Puzzle, Wordlist, Solved),
	print_puzzle(SolutionFile, Solved).

read_file(Filename, Content) :-
	open(Filename, read, Stream),
	read_lines(Stream, Content),
	close(Stream).

read_lines(Stream, Content) :-
	read_line(Stream, Line, Last),
	(   Last = true
	->  (   Line = []
	    ->  Content = []
	    ;   Content = [Line]
	    )
	;  Content = [Line|Content1],
	    read_lines(Stream, Content1)
	).

read_line(Stream, Line, Last) :-
	get_char(Stream, Char),
	(   Char = end_of_file
	->  Line = [],
	    Last = true
	; Char = '\n'
	->  Line = [],
	    Last = false
	;   Line = [Char|Line1],
	    read_line(Stream, Line1, Last)
	).

print_puzzle(SolutionFile, Puzzle) :-
	open(SolutionFile, write, Stream),
	maplist(print_row(Stream), Puzzle),
	close(Stream).

print_row(Stream, R) :-
	maplist(put_puzzle_char(Stream), R),
	nl(Stream).

put_puzzle_char(Stream, Char) :-
	(   var(Char)
	->  put_char(Stream, '_')
	;   put_char(Stream, Char)
	).

valid_puzzle([]).
valid_puzzle([R|Rs]) :-
	maplist(samelength(R), Rs).

samelength([], []).
samelength([_|L1], [_|L2]) :-
	same_length(L1, L2).

% Solution

% Takes a puzzle and a list of words as arguments
% Returns the filled puzzle.
solve_puzzle(Solution, [], Solution).
solve_puzzle(Puzzle, Wordlist, Solved):-
    % Takes a list of words as input
	% Returns an optimised version of the list
	filter_wordlist(Wordlist, Wordlist_Filtered),

	% Takes a puzzle as input
	% Returns all the horizontal word slots
	% Returns an updated version of the puzzle whose blank spaces are filled. 
	fill_horizontal(Puzzle, Slots_Horizontal, Puzzle1),

	% Takes the updated puzzle as input
	% Returns its flipped version
	flip(Puzzle1,0,Puzzle1_T),

	% Takes the transposed puzzle as input
	% Returns the new slots with vertical variables filled in
	fill_vertical(Puzzle1_T, Slots_Horizontal, Slots),

	% Takes the transposed puzzle as input
	% Returns the puzzle transposed back to the normal
	flip(Puzzle1_T,1,Final_Puzzle),

	% Takes the optimised list as input
	% Returns the filled-in slots
	insert(Wordlist_Filtered, Slots),
	
	% Takes the finalised puzzle as input
	% Return it as the solved puzzle
	solve_puzzle(Final_Puzzle, [], Solved),
	!.


% Word List Optimisation

% Takes a list of words as input
% Returns an optimised version of the list
filter_wordlist(Wordlist, Wordlist_Filtered):-
	% Takes a list of words as input
	% Returns the descending order sorted word list
	qsort(Wordlist, Wordlist_Sorted),

	% Takes the sorted word list as input
	% Returns the bucketed word lists
	bucket_wordlist(Wordlist_Sorted, Bucketed_Wordlist),

	% Takes the bucketed word list as input
	% Returns the ascending order sorted bucketed word list
	qsort_a(Bucketed_Wordlist, Bucketed_Wordlist_Sorted),

	% Takes the sorted bucketed word list as input
	% Returns the word list whose nesting layers are removed
	remove_layer(Bucketed_Wordlist_Sorted, Wordlist_Filtered).


% Takes the sorted word list as input
% Returns lists of words bucketed by length
bucket_wordlist([W|Ws], Bucketed_Wordlist):-length(W, L),
	b_l([W|Ws], L, [], [], Bucketed_Wordlist).

% Takes the sorted word list, word length, and two buckets of slots as input
% Returns lists of words bucketed by length
b_l([], _, [], Acc, Acc):- !.
b_l([], _, Acc_s, Acc_l, Bucketed_Wordlist):-

	% Takes a bucketed slot as input 
	% Returns a boolean value
	not(length(Acc_s, 0)),

	% Takes the large bucket of slots and small bucket of slots as input
	% Returns their concatenation.
	append(Acc_l, [Acc_s], Acc_l1),

	b_l([], _, [], Acc_l1, Bucketed_Wordlist).

b_l([W|Ws], Previous_Length, 
	Acc_s, Acc_l, Bucketed_Wordlist):-length(W, W_Length),
	(W_Length =:= Previous_Length -> 
		append(Acc_s, [W], Acc_s1),
		b_l(Ws, W_Length, 
			Acc_s1, Acc_l, Bucketed_Wordlist)
		;
		append(Acc_l, [Acc_s], Acc_l1),
		b_l(Ws, W_Length, 
			[W], Acc_l1, Bucketed_Wordlist)
	).

% Takes a list as input
% Removes its nesting layers
remove_layer(List, Removed):- rmv(List, [], Removed).
rmv([], Acc, Acc):- !.
rmv([T|Ts],Acc,Removed):-
	% Takes two lists as input
	% Returns the combined list
	append(Acc, T, Acc1),
	rmv(Ts, Acc1, Removed).


% Solve Slots

% Takes the transposed puzzle as input
% Returns the new slots with vertical variables filled in
fill_vertical([], Current_Slots, Current_Slots).
fill_vertical([R|Rs], Current_Slots, New_Slots):-
	
	% Takes rows as input
	% Returns slots
	bind_slot(R, Slots),

	% Takes two lists as input
	% Returns the combined list
	append(Current_Slots, Slots, Acc1),

	fill_vertical(Rs, Acc1, New_Slots).


% Takes a puzzle as input
% Returns all the horizontal word slots
% Returns an updated version of the puzzle whose blank spaces are filled. 
fill_horizontal(Puzzle, Slots, New_Puzzle):-fill_Hor(Puzzle, [], [], Slots, New_Puzzle).
fill_Hor([], Slot_Acc, New_Acc, Slot_Acc, New_Acc).
fill_Hor([R|Rs], Slot_Acc, New_Acc, Slots, New_Puzzle):-

	% Takes rows as input
	% Returns a filled row
	bind_puzzle(R, New_R),

	append(New_Acc, [New_R], New_Acc1),

	% Takes the filled low as input
	% Returns the slots from the row
	bind_slot(New_R, New_Slot),

	append(Slot_Acc, New_Slot, Slot_Acc1),
	fill_Hor(Rs, Slot_Acc1, 
		New_Acc1, Slots, New_Puzzle).


% Takes rows as input
% Returns a new puzzle whose blank spaces are filled.
bind_puzzle(R, New_Puzzle):- 
	% Takes rows and slots
	% Returns the slots with filled rows
	bind_log(R, [], New_Puzzle).
bind_log([], Acc, Acc):- !.
bind_log([Slot|Slots], Acc, R_log):-
	(Slot = '_' ->
		length(A, 1),
		append(Acc, A, Acc1),
		bind_log(Slots, Acc1, R_log)
		;
		append(Acc, [Slot], Acc1),
		bind_log(Slots, Acc1, R_log)
	).

% Takes rows as input
% Returns the list of buckets
bind_slot(R, Slots):- 
	b_s(R, [], [], Slots).
b_s([], [], Acc, Acc):- !.
b_s([], Current_Slot, Acc, Slots):-length(Current_Slot, Leng),
	(
		% Ensure the length is less than 2
		Leng >= 2 -> 
		append(Acc, [Current_Slot], Acc1),
		b_s([], [], Acc1, Slots)
		;
		b_s([], [], Acc, Slots)
	).
b_s([S|Ss], Current_Slot, Acc, Slots):-
	(
		S == # ->
		length(Current_Slot, Leng),
		(Leng >= 2 ->
			append(Acc, [Current_Slot], Acc1),
			b_s(Ss, [], Acc1, Slots)
			;
			b_s(Ss, [], Acc, Slots)
		)
		;
		append(Current_Slot, [S], Current_Slot1),
		b_s(Ss, Current_Slot1, Acc, Slots)
	).


% Word Insertion

%  Takes a list of words and slots as input
%  Binds the elements of the list to the slots.
insert([], _):- !.
insert([Word|Words], Current_Slotlist):-
	bind_word(Word, Current_Slotlist, Partial),
	insert(Words, Partial).

% Takes a word and buckets
% Returns buckets
bind_word(W, [S|Ss], Slots):- bind_w(W, [S|Ss], [], Slots).
bind_w(_, [], Acc, Acc):- !.
bind_w(W, [S|Ss], Acc, Slots):-
	(
		% Check unification
		W = S,
		append(Acc, [S], Acc1),
		append(Acc1, Ss, Acc2),
		bind_w(_, [], Acc2, Slots)
	)
	;
	(
		append(Acc, [S], Acc1),
		bind_w(W, Ss, Acc1, Slots)
	),
	not(length(Ss, 0)).



% Other Predicates

%  Quicksort
%  Extracted from http://kti.mff.cuni.cz/~bartak/prolog/sorting.html
pivot(_, [], [], []).
pivot(H, [X|T], [X|L], G):- 
	length(H, Hl),
	length(X, Xl),
	Xl =< Hl,
	pivot(H, T, L, G).
pivot(H, [X|T], L, [X|G]):- 
	length(H, Hl),
	length(X, Xl),
	Xl > Hl,
	pivot(H, T, L, G).

qsort(List, Sorted):-qsort_d(List, [], Sorted).
qsort_d([], Acc, Acc).
qsort_d([H|T], Acc, Sorted):-
	pivot(H, T, L1, L2),
	qsort_d(L1, Acc, Sorted1),
	qsort_d(L2, [H|Sorted1], Sorted).


%  Quicksort
%  Extracted from http://kti.mff.cuni.cz/~bartak/prolog/sorting.html
pivot_a(_, [], [], []).
pivot_a(H, [X|T], [X|L], G):- 
	length(H, Hl),
	length(X, Xl),
	Xl >= Hl,
	pivot_a(H, T, L, G).
pivot_a(H, [X|T], L, [X|G]):- 
	length(H, Hl),
	length(X, Xl),
	Xl < Hl,
	pivot_a(H, T, L, G).

qsort_a(List, Sorted):-q_sort(List, [], Sorted).
q_sort([], Acc, Acc).
q_sort([H|T], Acc, Sorted):-
	pivot_a(H, T, L1, L2),
	q_sort(L1, Acc, Sorted1),
	q_sort(L2, [H|Sorted1], Sorted).


% Takes a puzzle and which way of flipping
% Returns a flipped Puzzle
flip(Puzzle,Way,Flipped):-
(	Way =:= 0 ->
	flipA(Puzzle,Flipped)
;	flipB(Puzzle,Flipped)
).

flipA(A,B):-
	transpose(A,B).


flipB(A,B):-
	transpose(A,B1),
	transpose(B1,B2),
	transpose(B2,B).

