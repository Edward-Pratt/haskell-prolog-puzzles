number([], 0).
number(Digits, Number) :-
    atomic_list_concat(Digits, Atom),
    atom_number(Atom, Number).



generator3([AS, BS]) :-
    Digits = [1, 2, 3, 4, 5, 6, 7, 8, 9],
    subset(Digits, ASUnordered),
    subtract(Digits, ASUnordered, BSUnordered),
    ASUnordered \= [], BSUnordered \= [],
    permutation(ASUnordered, AS),
    permutation(BSUnordered, BS).


subset([], []).
subset([X|Xs], [X|Ys]) :-
    subset(Xs, Ys).
subset([_|Xs], Ys) :-
    subset(Xs, Ys).


selector3([AS,BS]) :-
    number(AS, ASnumber),
    number(BS, BSnumber),
    Product is ASnumber*BSnumber,
    firstIs4(Product),
    (ASnumber<BSnumber
    ->  M is mod(ASnumber, 10)
    ;   M is mod(BSnumber, 10)),
    M = 3,
    N is ASnumber + BSnumber + 100,
    palindrome(N),
    palindrome(Product).
	

% Used From Lectures
digits(N, [N]) :-
    N < 10.

% Used From Lectures
digits(N, W) :-
    N >= 10,
    div_mod(N, 10, D, M),
    digits(D, R),
    append(R, [M], W).

% Used From Lectures
div_mod(A, B, D, M) :-
    D is A div B,
    M is A mod B.


firstIs4(Number) :-
    digits(Number, X),
    X = [4|_].



palindrome(List) :-
    digits(List, ListDigits),
    reverse(ListDigits, ListDigits).


main :- 
    generator3(X), selector3(X), write(X).

