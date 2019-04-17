read_input(File, N, Teams) :-
    open(File, read, Stream),
    read_line_to_codes(Stream, Line),
    atom_codes(Atom, Line),
    atom_number(Atom, N),
    read_lines(Stream, N, Teams).

read_lines(Stream, N, Teams) :-
    ( N == 0 -> Teams = []
    ; N > 0  -> read_line(Stream, Team),
                Nm1 is N-1,
                read_lines(Stream, Nm1, RestTeams),
                Teams = [Team | RestTeams]).

read_line(Stream, team(Name, P, A, B)) :-
    read_line_to_codes(Stream, Line),
    atom_codes(Atom, Line),
    atomic_list_concat([Name | Atoms], ' ', Atom),
    maplist(atom_number, Atoms, [P, A, B]).

match(_,_,_,_).

find_stats(team(_,A, B, C), [], A, B, C).
find_stats(team(Name, A, B, C), [match(N1, N2, G1, G2)|Matches], M, GY, GK):-
  (( Name = N1 ->
     NewGY is GY + G1,
     NewGK is GK + G2
  ;  Name = N2 ->
     NewGY is GY + G2,
     NewGK is GK + G1) ->
     NewM is M+1,
     find_stats(team(Name, A, B, C), Matches, NewM, NewGY, NewGK)
  ;  find_stats(team(Name, A, B, C), Matches, M, GY, GK) ).

check([], _).
check([R|Results], Matches) :-
  find_stats(R, Matches, 0, 0, 0),
  check(Results, Matches).

mundial(File, Matches):-
  read_input(File, _N, Teams),
  check(Teams, Matches).
