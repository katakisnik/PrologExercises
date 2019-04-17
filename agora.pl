gcd(X, 0, X):- !.
gcd(0, X, X):- !.
gcd(X, Y, D):- X > Y, !, Z is X mod Y, gcd(Y, Z, D).
gcd(X, Y, D):- Z is Y mod X, gcd(X, Z, D).

lcm(X, Y, LCM):-
  gcd(X, Y, GCD), LCM is (X//GCD)*Y.

listlcm(L, Result):-helper1(L, Result, 1).
helper1([], Result, Result).
helper1([H|T], Result, LCM1) :-
  lcm(H, LCM1, LCM), helper1(T, Result, LCM).


reverse([],Z,Z).
reverse([H|T],Z,Acc) :- reverse(T,Z,[H|Acc]).

left(L, [1|L1]):-helper(L, L1, 1).
helper([], [], _).
helper([_H1|[]], _, _).
helper([H1|T1], [LCM|L1], A) :-
  lcm(H1, A, LCM), helper(T1, L1, LCM).

right(L, L1):-
  reverse(L, X, []),
  left(X, L2),
  reverse(L2, L1, []).

make([], [], []).
make([H1|T1], [H2|T2], [LCM|T3]) :-
  lcm(H1, H2, LCM), make(T1, T2, T3).

indexOf([Element|_], Element, 0):- !.
indexOf([_|Tail], Element, Index):-
  indexOf(Tail, Element, Index1),
  !,
  Index is Index1+1.

read_input(File, N, L) :-
  open(File, read, Stream),
  read_line(Stream, N),
  read_line(Stream, L).

read_line(Stream, List) :-
  read_line_to_codes(Stream, Line),
  ( Line = [] -> List = []
  ; atom_codes(A, Line),
    atomic_list_concat(As, ' ', A),
    maplist(atom_number, As, List)
  ).

village(A, B, C, D) :-
  ( A =:= B -> D is -1
  ; D is C).

together(L, L1, L2, L3, Answer, Index) :-
  listlcm(L, Result),
  left(L, L1),
  right(L, L2),
  make(L1, L2, L3),
  min_list(L3, Answer),
  indexOf(L3, Answer, Index1),
  village(Answer, Result, Index1, Index2),
  Index is Index2 + 1.


agora(File, Answer, Index) :- once(solve(File, Answer, Index)).
solve(File, Answer, Index) :- read_input(File, _N, L), together(L, _L1, _L2, _L3, Answer, Index).
