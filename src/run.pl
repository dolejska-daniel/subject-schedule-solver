/**
 * 
 */

% Load local modules.
:- use_module(src/io).
:- use_module(src/logic).
:- use_module(src/utils).


main:-
     % load CLI args
     arg_value("--load-from", none, SubjectsFile),
     arg_value("--save-to", none, SolutionsFile),
     % load subjects from file
     input(SubjectsFile, _),
     % generate all possible solutions
     findall(Solution, solution(Solution), Solutions),
     debug(general, "Found following solutions: ~w.", [Solutions]),
     % store the results
     output(SolutionsFile, Solutions),
     % exit the program
     halt.

%-------------------------------------------------------dd--
%   Input related definitions
%-------------------------------------------------------dd--

input(none, Subjects):-
     load_subjects(current_input, Subjects).

input(File, Subjects):-
     load_subjects_from_file(File, Subjects).

%-------------------------------------------------------dd--
%   Output related definitions
%-------------------------------------------------------dd--

output(none, Solutions):-
     print_solutions(Solutions), !.

output(File, print_solutions(Solutions)):-
     save_solutions(File, Solutions).
