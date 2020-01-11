%- N, a nonnegative integer specifying the size of the square grid.
%- T, a list of N lists, each representing a row of the square grid. 
%     Each row is represented by a list of N distinct integers from 1 through N. 
%     The corresponding columns also contain all the integers from 1 through N.
%- C, a structure with function symbol counts and arity 4. Its arguments are all lists of N integers, 
%     and represent the tower counts for the top, bottom, left, and right edges, respectively.  
%- tower(N, T, C) => Given the unique 1 to N counts (N) or rows of towers (T) and return the boundry counts (C)

%----------------- TOWER
%- assume preconditions hold
tower(N, T, C) :- 
	%- from piazza
	C = counts(TOP, BOTTOM, LEFT, RIGHT), 
	%- Checks if all are lists of N integers
	checkCounts(C, N),
	%- maplist input validation (length, uniqueness, within domain)
	maplist(checkLength(N),T), 
	maplist(fd_all_different,T), 
	maplist(checkDomain(N), T), 
	%- transpose used to check all rows must be different, again, for uniqueness
	transpose(T,TRANSPOSED),
	%- fd_all_different constrains all variables in TRANSPOSED to take distinct values.
    maplist(fd_all_different,TRANSPOSED),
    %- check LEFT & RIGHT column's correctness of original matrix
    check(T, LEFT, N),
	checkOppositeSide(T, RIGHT, N),
	 %- check TOP & BOTTOM column's correctness of transposed matrix
	check(TRANSPOSED, TOP, N),
	checkOppositeSide(TRANSPOSED, BOTTOM, N),
	%- fd_labeling triggers the external constraint solver
	maplist(fd_labeling, T).

checkCounts(counts(TOP, BOTTOM, LEFT, RIGHT), N) :-
	length(TOP, N),
	length(BOTTOM, N),
	length(LEFT, N),
	length(RIGHT, N).

checkLength(N,LENGTH) :- length(LENGTH,N).

checkDomain(N, []).
checkDomain(N, [HEAD | TAIL]) :-
	%- available in GNU Prolog to express a range for constrained variables
	fd_domain(HEAD, 1, N),
	checkDomain(N, TAIL).

%-----           transpose code obtained from SWI-prolog         -----
%----- per Piazza https://piazza.com/class/jqr2kb45gw2jt?cid=221 -----

transpose([], []).
transpose([F|Fs], Ts) :-
	transpose(F, [F|Fs], Ts).

transpose([], _, []).
transpose([_|Rs], Ms, [Ts|Tss]) :-
	lists_firsts_rests(Ms, Ts, Ms1),
	transpose(Rs, Ms1, Tss).
	
lists_firsts_rests([], [], []).
lists_firsts_rests([[F|Os]|Rest], [F|Fs], [Os|Oss]) :-
	lists_firsts_rests(Rest, Fs, Oss).
%----------------------------------------------------------------------
check([], _, N).
check([HEAD | TAIL], [FIRST | REST], N) :-	
	count(HEAD, COUNTER, 0),
	FIRST is COUNTER,
	check(TAIL, REST, N).
	
checkOppositeSide([], _, N).
checkOppositeSide([HEAD | TAIL], [FIRST | REST], N) :-
	reverse(HEAD, RHEAD),
	count(RHEAD, COUNTER, 0),
	FIRST = COUNTER,
	checkOppositeSide(TAIL, REST, N).
	
count([],0,_).
count([HEAD | TAIL],C,M) :- HEAD #< M, count(TAIL,C,M).
count([HEAD | TAIL],C,M) :- HEAD #> M, C2 #= C - 1, count(TAIL,C2,HEAD).

%----------------- PLAIN TOWER

count_right_bottom(L, N):- reverse(L,Ll), count_left_top(Ll, N).
 
count_left_top([],0):- !.
count_left_top([_], 1):- !.
count_left_top([H1,H2], N):- H1>H2, N is 1,!.
count_left_top([H1,H2], N):- H1<H2, N is 2,!.
 
count_left_top([H1,H2|T], N):- H1>H2, count_left_top([H1|T], N1), N is N1.
count_left_top([H1,H2|T], N):- H1<H2, count_left_top([H2|T], N1), N is N1 +1.

% generate a list in descending order, from discussion section
generate_desc_list(0, []).   
generate_desc_list(N, Y):- N > 0, NN is N-1, Y = [N|T], generate_desc_list(NN, T).
 
permutate_new_list(N, L) :- generate_desc_list(N, P), reverse(P, L).

p_check_left(_,[],[]).
p_check_left(N,[H1|T1],[H2|T2]) :- 
    length(L1, N),
    permutate_new_list(N, L1),
    permutation(L1,H1),
    count_left_top(H1,H2),
    p_check_left(N,T1,T2).

p_check_top(_,[],[]).
p_check_top(N,[H1|T1],[H2|T2]) :- 
    length(L1, N),
    permutate_new_list(N, L1),
    permutation(L1,H1),
    count_left_top(H1,H2),
    p_check_top(N,T1,T2).
 
p_check_right(_,[],[]).
p_check_right(N,[H1|T1],[H2|T2]) :- 
    length(L1, N),
    permutate_new_list(N, L1),
    permutation(L1,H1),
    count_right_bottom(H1,H2),
    p_check_right(N,T1,T2).

p_check_bottom(_,[],[]).
p_check_bottom(N,[H1|T1],[H2|T2]) :- 
    length(L1, N),
    permutate_new_list(N, L1),
    permutation(L1,H1),
    count_right_bottom(H1,H2),
    p_check_bottom(N,T1,T2).
  
% base case
plain_tower(0, [], counts(TOP, BOTTOM, LEFT, RIGHT)):- !.
plain_tower(N, TT, C):-
    C = counts(TOP, BOTTOM, LEFT, RIGHT),
    length(TT, N),
    p_check_left(N,TT, LEFT),
    p_check_right(N,TT, RIGHT),
    transpose(TT, TTC),
    p_check_top(N,TTC, TOP),
    p_check_bottom(N,TTC, BOTTOM).

%----------------- STATISTICS 
tower_time(Time) :-
	statistics(runtime, [T1|_]),
	tower(5, T, counts([4, 4, 2, 2, 1], [2, 1, 2, 3, 3], [5, 2, 2, 1, 2], [1, 2, 3, 4, 3])),
	statistics(runtime, [T2|_]),
	Time is T2 - T1.

plain_tower_time(Time) :-
	statistics(runtime, [T1|_]),
	plain_tower(5, T, counts([4, 4, 2, 2, 1], [2, 1, 2, 3, 3], [5, 2, 2, 1, 2], [1, 2, 3, 4, 3])),
	statistics(runtime, [T2|_]),
	Time is T2 - T1.

% issue with division 0
speedup(T) :- tower_time(TT), plain_tower_time(PTT), T is PTT/TT.

%----------------- AMBIGUOUS
ambiguous(N, C, T1, T2) :- tower(N, T1, C), tower(N, T2, C), T1 \= T2.
	 
