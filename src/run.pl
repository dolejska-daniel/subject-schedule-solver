/**
 * 
 */

% Load local modules.
:- use_module(src/io).
:- use_module(src/logic).
:- use_module(src/utils).


main:-
     % load CLI args
     arg_value("--load-from", "example/subjects.json", SubjectsFile),
     arg_value("--save-to", none, SolutionsFile),
     % load subjects from file
     load_subjects(SubjectsFile, _),
     % generate all possible solutions
     findall(Solution, solution(Solution), Solutions),
     debug(general, "Found following solutions: ~w.", [Solutions]),
     % store the results
     output(SolutionsFile, Solutions),
     % exit the program
     halt.

output(none, Solutions):-
     print_solutions(Solutions), !.

output(File, print_solutions(Solutions)):-
     save_solutions(File, Solutions).
