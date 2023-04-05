
lpd(CUR, RET) :- lpd_table(CUR, RET), !.
nth_lpd(BASE, CUR, MAX_N) :- CUR > MAX_N, !.
nth_lpd(BASE, CUR, MAX_N) :- assert(lpd_table(CUR, BASE)), C1 is CUR + BASE, nth_lpd(BASE, C1, MAX_N).

make_sieve(CUR, MAX_N) :- CUR * CUR > MAX_N, !.
make_sieve(CUR, MAX_N) :- lpd_table(CUR, _), !, CUR1 is CUR + 1, make_sieve(CUR1, MAX_N).
make_sieve(CUR, MAX_N) :- nth_lpd(CUR, CUR, MAX_N), !,  CUR1 is CUR + 1, make_sieve(CUR1, MAX_N).

init(MAX_N) :- make_sieve(2, MAX_N).

prime(N) :- lpd(N, N), !.
prime(N) :- \+ lpd(N, _).
composite(N) :- \+ prime(N).

sorted([H1 | []]) :- !.
sorted([H1, H2 | T]) :- H1 =< H2, sorted([H2 | T]).

prime_divisors(1, []) :- !.
prime_divisors(NUM, [PRIME | T]) :- number(NUM), lpd(NUM, PRIME), !, DIV is NUM / PRIME, prime_divisors(DIV, T).
prime_divisors(NUM, [NUM]) :- number(NUM), !.
% :NOTE: [X, Y, Z].
prime_divisors(NUM, [H | T]) :- \+ number(NUM), !, sorted([H | T]), prime_divisors(NUM1, T), NUM is NUM1 * H.

smart_concat([], [], []) :- !.
smart_concat([], V, V) :- !.
smart_concat(V, [], V) :- !.
smart_concat([H | T1], [H | T2], [H | T]) :- !, smart_concat(T1, T2, T).
smart_concat([H1 | T1], [H2 | T2], [H1 | T]) :- H1 < H2, !, smart_concat(T1, [H2 | T2], T).
smart_concat([H1 | T1], [H2 | T2], [H2 | T]) :- smart_concat([H1|T1] , T2, T).

lcm(A, B, LCM) :-
prime_divisors(A, DIVA),
prime_divisors(B, DIVB),
smart_concat(DIVA, DIVB, DIVLCM),
prime_divisors(LCM, DIVLCM).
