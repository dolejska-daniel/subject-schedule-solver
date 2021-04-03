/**
 * 
 */
:- module(logic, [
    %
    not_same/2,
    not_overlapping/2,
    not_overlapping_with_any/2,
    %
    registered/1,
    register/1,
    register_all/2,
    %
    solution/1,
    solution_to_json/2,
    solutions_to_json/2
]).

% Load local modules.
:- use_module(src/data).


%===========================================================================dd==
%   CONDITION AND STATE RELATION DEFINITIONS
%===========================================================================dd==

/**
 * not_overlapping(+Subject1:subject, +Subject2:subject)
 * 
 * Verifies that Subject1 is a different subject than Subject2.
 */
not_same(Subject1, Subject2):-
    subject_name(Subject1, Name1), subject_name(Subject2, Name2),
    Name1 \= Name2.

/**
 * not_overlapping(+Subject1:subject, +Subject2:subject)
 * 
 * Verifies that there is no time overlap between Subject1 and Subject2.
 */
not_overlapping(Subject1, Subject2):-
    % there is a different subject on any day
    not_same(Subject1, Subject2),
    % starting at a earlier time
    subject_is_earlier_without_overlap(Subject1, Subject2), !.

not_overlapping(Subject1, Subject2):-
    % there is a different subject on any day
    not_same(Subject1, Subject2),
    % starting at a later time
    subject_is_later_without_overlap(Subject1, Subject2), !.

not_overlapping(Subject1, Subject2):-
    % there is a different subject
    not_same(Subject1, Subject2),
    % on a different day
    subject_day(Subject1, Day1), subject_day(Subject2, Day2),
    Day1 \= Day2, !.

/**
 * not_overlapping_with_any(+Subject:subject, +Subjects:list)
 * 
 * Verifies that there is no time overlap between provided Subject and any subjects in Subjects.
 */
not_overlapping_with_any(Subject, [OtherSubject|OtherSubjects]):-
    not_overlapping(Subject, OtherSubject),
    not_overlapping_with_any(Subject, OtherSubjects).

not_overlapping_with_any(_, []).


%===========================================================================dd==
%   REGISTERED STRUCTURE
%===========================================================================dd==

:- dynamic registered/1.

%-------------------------------------------------------dd--
%   Operations and field access
%-------------------------------------------------------dd--

/**
 * register(+Subject:subject)
 * 
 * Registers the subject option provided in Subject.
 * Evaluates all conditions and passes only if the subject can in fact be registered.
 */
register(Subject):-
    % there are other registered subjects
    setof(RegisteredSubject, registered(RegisteredSubject), RegisteredSubjects),
    debug(register, "Currently registered ~w.", [RegisteredSubjects]),
    % these two subjects are not overlapping
    not_overlapping_with_any(Subject, RegisteredSubjects),
    % iff that's true then register this subject too
    assertz(registered(Subject)),
    debug(register, "Just registered ~w.", [Subject]).

/**
 * Passes when there is no other registered subject yet.
 */
register(Subject):-
    % there are no registered subjects yet
    findall(RegisteredSubject, registered(RegisteredSubject), []),
    % iff that's true then register this subject as a first one
    assertz(registered(Subject)).

/**
 * register_all(+Subjects:list, -RegisteredSubjects:list)
 * 
 * Registers matching subject option of all Subjects and retuns it in RegisteredSubjects.
 */
register_all([Subject|Subjects], [Subject|ResultTail]):-
    register(Subject),
    register_all(Subjects, ResultTail).

register_all([], []).


%===========================================================================dd==
%   SOLUTION STRUCTURE
%===========================================================================dd==

/**
 * solution(-RegisteredSubjects:list)
 * 
 * Creates a list of subject options that all pass required conditions in RegisteredSubjects.
 */
solution(RegisteredSubjects):-
    % create a set of subjects names - unique subjects
    setof(Subject, subject_name(Subject), SubjectNames),
    debug(general, "Validating subjects: ~w.", [SubjectNames]),
    % create a list of possible combinations of subject options (different days/hours)
    subject_options(SubjectNames, Subjects),
    % remove potentially any previously registered subjects
    retractall(registered(_)),
    debug(general, "Now validating subject options: ~w.", [Subjects]),
    % try match the conditions for all the provided subjects
    register_all(Subjects, RegisteredSubjects),
    debug(general, "Possible solution: ~w.", [RegisteredSubjects]).

%-------------------------------------------------------dd--
%   JSON related definitions
%-------------------------------------------------------dd--

/**
 * solution_to_json(+Solution:list, -JsonSubjects:list)
 * 
 * Creates a list of matching JSON object representation of subjects from Solution in JsonSubjects.
 */
solution_to_json([Subject|OtherSubjects], [JsonSubject|OtherJsonSubjects]):-
    subject_to_json(Subject, JsonSubject),
    solution_to_json(OtherSubjects, OtherJsonSubjects).

solution_to_json([], []).

/**
 * solutions_to_json(+Solutions:list, -JsonSolutions:list)
 * 
 * Creates a list of matching JSON object representation of solutions from Solutions in JsonSolutions.
 */
solutions_to_json([Solution|OtherSolutions], [JsonSolution|OtherJsonSolutions]):-
    solution_to_json(Solution, JsonSolution),
    solutions_to_json(OtherSolutions, OtherJsonSolutions).

solutions_to_json([], []).
