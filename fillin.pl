% Made by Shixun Liu (StudentID:766799).
% The second project of DP subject.
% This program solves the fillin puzzles

% Load the correct 'transpose' predicate.
:- ensure_loaded(library(clpfd)).

%----------------------------------------------------%
%------- Implementation point of the program --------%
%----------------------------------------------------%

% The implementation point of the program
% PuzzleFile(Read): The file contains the puzzle.
% WordlistFile(Read): The file contains the words to fill in the above puzzle
% SolutionFile(Write): Write soluction to this file
main(PuzzleFile, WordlistFile, SolutionFile) :-

	% Read in puzzle & wordlist file
	% The contents are binded to variable 'Puzzle' & 'WordList'
	read_file(PuzzleFile, Puzzle),
	read_file(WordlistFile, Wordlist),

	% Ensure the input puzzle is valid
	% namely each line of puzzle has the same length
	valid_puzzle(Puzzle),

	% Based on the Puzzle & WordList, get the solution, binded to 'Solved'
	solve_puzzle(Puzzle, Wordlist, Solved),

	% Output the solution to the specified file
	print_puzzle(SolutionFile, Solved).

%----------------------------------------------------%
%------------------- File Input ---------------------%
%----------------------------------------------------%

% Read specified file,the contents are binded to variable 'Content'
read_file(Filename, Content) :-
	open(Filename, read, Stream),
	read_lines(Stream, Content),
	close(Stream).

% Get all lines of the file cont
% The 'Content' format is [line_1,line_2,...,line_n]
% format of line is [char_1,char_2,...,char_n]
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

% Read a line from file contents into list, each element is char
% It will only read one line from contents, either the last line or not
% If is last line, indicate with 'true', otherwise 'false'
% The format of 'Line' is [char_1,char_2,...,char_n] 
read_line(Stream, Line, Last) :-
	get_char(Stream, Char),
	(   Char = end_of_file
	->  Line = [],
	    Last = true
	; 	Char = '\n'
	->  Line = [],
	    Last = false
	;   Line = [Char|Line1],
	    read_line(Stream, Line1, Last)
	).

%----------------------------------------------------%
%------------------- File Output --------------------%
%----------------------------------------------------%

% 'Puzzle' is the solved one with words
% print each row of puzzle into stream, then write stream into file
print_puzzle(SolutionFile, Puzzle) :-
	open(SolutionFile, write, Stream),
	maplist(print_row(Stream), Puzzle),
	close(Stream).

% print a row into stream
print_row(Stream, Row) :-
	maplist(put_puzzle_char(Stream), Row),
	nl(Stream).

% if The variable 'Char' is uninstantiated
% put '_' into Stream, otherwise the instantiated value
put_puzzle_char(Stream, Char) :-
	(   var(Char)
	->  put_char(Stream, '_')
	;   put_char(Stream, Char)
	).

% valid if rest of rows have the some length of first row
valid_puzzle([]).
valid_puzzle([Row|Rows]) :-
	maplist(samelength(Row), Rows).

% check if the length of two lists are same 
samelength([], []).
samelength([_|L1], [_|L2]) :-
	same_length(L1, L2).

%----------------------------------------------------%
%--------------------- Solution ---------------------%
%----------------------------------------------------%

% Start sloving the puzzle
%solve_puzzle(Puzzle_Blank, WordList, Puzzle_Filled):-

	%Resort the wordlist, making 
%	process_wordlist(WordList,WordList_New).


process_wordlist(WordlistFile,WordList_New):-

	read_file(WordlistFile, Wordlist),
	
	% sort the wordlist based on each word length, decending order
	quick_sort_decend(Wordlist, WordList_decent_order),

	% categorise the word based on its length
	categorise_list(WordList_decent_order,Wordlist_New).

%------------------------------------------------------------------%
%------- Rules used for categorise the elements of a list ----------%
%------------------------------------------------------------------%

% For a sorted list, get the first several elements whose value are same
% First_Element: the first ele of a list
% Acc: Accumulator used to store the element
% Eg. [5,5,3,3,1,1] -> [5,5]
extract_first_N_same_elements([First_Element|Tail],Acc,Result):-
	   Acc = []
	-> extract_first_N_same_elements(Tail,[First_Element],Result)
	;
	   member(First_Element,Acc),
	   extract_first_N_same_elements(Tail,[First_Element|Acc],Result).
extract_first_N_same_elements(_,Acc,Acc).

% categorise the elements of sorted list, the same elements are binded in a list
% Acc: Accumulator used to store the sublist
% Eg. [5,5,5,3,3,6,6,1,1,1,1] -> [[1,1,1,1],[6,6],[3,3],[5,5,5]]
categorise_list([],Acc,Acc):-!.
categorise_list(List,Acc,Result):-
	extract_first_N_same_elements(List,[],Temp),
	length(Temp,N),
	remove_First_N_Elements(List,N,Rest),
	categorise_list(Rest,[Temp|Acc],Result).

% Remove first N elements from list.
% List: The list to move element from
% Tail: The tail of a list
% N: Number of first elements
% Result: The list without first N elements
% Eg. [1,2,3,4,5] -> [3,4,5] when N is 2
remove_First_N_Elements(List,0,List).
remove_First_N_Elements([_|Tail],N,Result):-
	Temp is N-1,
	remove_First_N_Elements(Tail,Temp,Result).

%------------------------------------------------------------------%
%------------------------ Quicksort -------------------------------%
%-- http://kti.mff.cuni.cz/~bartak/prolog/sorting.html#quick-------%

quick_sort_decend(List,Sorted):-
	q_sort(List,[],Sorted).
q_sort([],Acc,Acc).
q_sort([H|T],Acc,Sorted):-
	pivoting(H,T,L1,L2),
	q_sort(L1,Acc,Sorted1),
	q_sort(L2,[H|Sorted1],Sorted).

pivoting(_,[],[],[]).
pivoting(H,[X|T],[X|L],G):-
	length(X,X_length),
	length(H,H_length),
	X_length=<H_length,
	pivoting(H,T,L,G).
pivoting(H,[X|T],L,[X|G]):-
	length(X,X_length),
	length(H,H_length),
	X_length>H_length,
	pivoting(H,T,L,G).

