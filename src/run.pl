/**
 * 
 */

% Load local modules.
:- use_module(io).
:- use_module(logic).

:- use_module(data).
:- retractall(subject(_, _, _)).
% iel
:- assertz(subject('IEL', 'Monday',   interval(time(10, 00), time(11, 50)))).
:- assertz(subject('IEL', 'Tuesday',  interval(time(10, 00), time(11, 50)))).
% ida
:- assertz(subject('IDA', 'Monday',   interval(time(11, 00), time(12, 50)))).
:- assertz(subject('IDA', 'Tuesday',  interval(time(08, 00), time(10, 50)))).
:- assertz(subject('IDA', 'Thursday', interval(time(08, 00), time(10, 50)))).

main:-
     findall(Solution, solution(Solution), Solutions),
     debug(general, "Found following solutions: ~w.", [Solutions]),
     save_solutions("solutions.json", Solutions),
     print_solutions(Solutions),
     halt.
