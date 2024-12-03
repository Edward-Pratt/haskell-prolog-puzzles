mondays(D, S, [M|MS])
    :- firstMonday(S, M),
       numOfMondays(D, M, [M|MS]),
       between(28, 31, D),
       between(1, 7, S).

numOfMondays(D, M, [])
    :- M > D,
       !.
numOfMondays(D, M, [M|MS])
    :- M =< D,
       Next_Monday is M + 7,
       numOfMondays(D, Next_Monday, MS).

firstMonday(S, M)
    :- M is (8 - S) mod 7 + 1.


saturdays(D, S, Saturdays) :-
    firstSaturday(S, SA),
    numOfSaturdays(D, SA, Saturdays).


numOfSaturdays(D, SA, []) :-
    SA > D,
    !.
numOfSaturdays(D, SA, [SA|SAS]) :-
    SA =< D,
    NextSaturday is SA + 7,
    numOfSaturdays(D, NextSaturday, SAS).


firstSaturday(S, SA) :-
    SA is (13 - S) mod 7 + 1.



generator4([[M1,D1,S1],[M2,D2,S2]]) :-
    between(1,12,M1),
    (M1<12
    ->  M2 is M1+1
   	;   M2 is 1),
    daysInMonth(M1, D1),
    daysInMonth(M2, D2),
	between(1,7,S1),
	S2 is (S1+D1-1) mod 7 + 1.



selector4([[M1, D1, S1], [M2, D2, S2]]) :-
    T1 = D1,
    U1 = D2,
    lettersInMonth(M1, T2),
    lettersInMonth(M2, U2),
    

	days(D1, NumDays1),
    days(D2, NumDays2),
    countPrimesInList(NumDays1, T3),
    countPrimesInList(NumDays2, U3),
    
    mondays(D1, S1, Mondays1),
    mondays(D2, S2, Mondays2),
    length(Mondays1, T4),
    length(Mondays2, U4),
    

    saturdays(D1, S1, Saturdays1),
    countPrimesInList(Saturdays1, T5),
    saturdays(D2, S2, Saturdays2),
    countPrimesInList(Saturdays2, U5),


    T1 \= U1, T2 \= U2, T3 \= U3, T4 \= U4, T5 \= U5,
    SumT is T1 + T2 + T3 + T4 + T5,
    SumU is U1 + U2 + U3 + U4 + U5,
    prime(SumT), prime(SumU).

days(1, [1]).
days(N, [N|Rest]) :-
    N > 1,
    N1 is N - 1,
	days(N1, Rest).


lettersInMonth(1,7).
lettersInMonth(2,8).
lettersInMonth(3,5).               
lettersInMonth(4,5).
lettersInMonth(5,3).               
lettersInMonth(6,4).               
lettersInMonth(7,4).               
lettersInMonth(8,6).
lettersInMonth(9,9).               
lettersInMonth(10,7).               
lettersInMonth(11,8).               
lettersInMonth(12,8).


countPrimesInList([], 0).
countPrimesInList([X|XS], Count) :-
    countPrimesInList(XS, RestCount),
    (prime(X) -> Count is RestCount + 1 ; Count is RestCount).


% Used From Lectures               
prime(N) :-
    N > 1,
    \+ factorisable(2, N).

% Used From Lectures
factorisable(F, N) :-
    F * F =< N,
    0 is N mod F.

% Used From Lectures
factorisable(F, N) :-
    F * F =< N,
    F1 is F + 1,
    factorisable(F1, N).               
               
               
daysInMonth(1,31).
daysInMonth(2,28).
daysInMonth(2,29).
daysInMonth(3,31).
daysInMonth(4,30).
daysInMonth(5,31).
daysInMonth(6,30).
daysInMonth(7,31).
daysInMonth(8,31).
daysInMonth(9,30).
daysInMonth(10,31).
daysInMonth(11,30).
daysInMonth(12,31).


main :- 
    generator4(X), selector4(X), write(X).


