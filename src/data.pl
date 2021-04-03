/**
 * 
 */
:- module(data, [
    %
    day/1,
    %
    hour/1,
    minute/1,
    time/2,
    time_hour/2,
    time_minute/2,
    time_minutes/2,
    earlier/2,
    later/2,
    time_to_string/2,
    time_from_atom/2,
    %
    interval/2,
    interval_from/2,
    interval_to/2,
    earlier_no_overlap/2,
    later_no_overlap/2,
    %
    subject/3,
    subject_name/1,
    subject_name/2,
    subject_day/2,
    subject_interval/2,
    subject_interval_from/2,
    subject_interval_to/2,
    subject_option/2,
    subject_options/2,
    subject_is_earlier_without_overlap/2,
    subject_is_later_without_overlap/2,
    subject_to_json/2,
    subject_from_json/2
]).

% Load libraries.
:- use_module(library(http/json_convert)).


%===========================================================================dd==
%   DAY STRUCTURE
%===========================================================================dd==

day('Monday').
day('Wednesday').
day('Tuesday').
day('Thursday').
day('Friday').
day('Saturday').
day('Sunday').


%===========================================================================dd==
%   TIME STRUCTURE
%===========================================================================dd==

hour(H):- integer(H), H >= 0, H < 24.
minute(M):- integer(M), M >= 0, M < 60.
time(H, M):- hour(H), minute(M).

%-------------------------------------------------------dd--
%   Operations and field access
%-------------------------------------------------------dd--

time_hour(time(Hour, _), Hour).
time_minute(time(_, Minute), Minute).
time_minutes(time(Hour, Minute), Minutes):-
    Minutes is Hour * 60 + Minute.

%-------------------------------------------------------dd--
%   Relation definitions
%-------------------------------------------------------dd--

/**
 * earlier(+Time1:time, +Time2:time)
 * 
 * Passes when Time1 occured earlier than Time2 (e.g., time(11, 00) < time(12, 00), time(21, 59) < time(22, 00)).
 */
earlier(Time1, Time2):-
    time_minutes(Time1, Minutes1),
    time_minutes(Time2, Minutes2),
    Minutes1 < Minutes2.

/**
 * later(+Time1:time, +Time2:time)
 * 
 * Passes when Time1 occured later than Time2 (e.g., time(12, 00) > time(11, 00), time(22, 00) > time(21, 59)).
 */
later(Time1, Time2):-
    time_minutes(Time1, Minutes1),
    time_minutes(Time2, Minutes2),
    Minutes1 > Minutes2.

%-------------------------------------------------------dd--
%   JSON related definitions
%-------------------------------------------------------dd--

/**
 * time_to_string(+Time:time, -StringTime:string)
 * 
 * Creates matching string representation of Time in StringTime.
 */
time_to_string(time(Hour, Minute), StringTime):-
    format(atom(FormattedStringTime), "~|~`0t~w~2|:~|~`0t~w~5|", [Hour, Minute]),
    term_string(FormattedStringTime, StringTime, [quoted(false)]).

/**
 * time_from_atom(+AtomTime:atom, -Time:time)
 * 
 * Creates time functor from AtomTime in Time.
 */
time_from_atom(AtomTime, time(Hour, Minute)):-
    % split string with "="
    sub_atom(AtomTime, Before, _, After, ":"), !,
    % unify prefix to argument name
    sub_atom(AtomTime, 0, Before, _, AtomHour),
    % unify suffix to argument value
    sub_atom(AtomTime, _, After, 0, AtomMinute),
    % convert atoms to integers
    atom_number(AtomHour, Hour),
    atom_number(AtomMinute, Minute).


%===========================================================================dd==
%   TIME INTERVAL STRUCTURE
%===========================================================================dd==

interval(From, To):- time(From), time(To), earlier(From, To).

%-------------------------------------------------------dd--
%   Operations and field access
%-------------------------------------------------------dd--

interval_from(interval(From, _), From).
interval_to(interval(_, To), To).

%-------------------------------------------------------dd--
%   Relation definitions
%-------------------------------------------------------dd--

/**
 * earlier_no_overlap(+Interval1:interval, +Interval2:interval)
 * 
 * Passes when Interval1 occurs entirely before Interval2 (Interval1.To < Interval2.From).
 */
earlier_no_overlap(Interval1, Interval2):-
    interval_to(Interval1, To1),
    interval_from(Interval2, From2),
    earlier(To1, From2).

/**
 * later_no_overlap(+Interval1:interval, +Interval2:interval)
 * 
 * Passes when Interval1 occurs entirely after Interval2 (Interval1.From > Interval2.To).
 */
later_no_overlap(Interval1, Interval2):-
    interval_to(Interval1, To1),
    interval_from(Interval2, From2),
    later(From2, To1).


%===========================================================================dd==
%   SUBJECT STRUCTURE
%===========================================================================dd==

:- dynamic subject/3.

%-------------------------------------------------------dd--
%   Operations and field access
%-------------------------------------------------------dd--

subject_name(Name):- subject(Name, Day, interval(From, To)), day(Day), time(From), time(To).

subject_name(subject(Name, _, _), Name).
subject_day(subject(_, Day, _), Day).
subject_interval(subject(_, _, Interval), Interval).
subject_interval_from(subject(_, _, interval(From, _)), From).
subject_interval_to(subject(_, _, interval(_, To)), To).

/**
 * subject_option(+Name:string, -SubjectOption:subject)
 * 
 * Selects subject based on provided Name.
 */
subject_option(Name, subject(Name, Day, Interval)):-
    subject(Name, Day, Interval).

/**
 * subject_options(+SubjectNames:set, -SubjectOptions:list)
 * 
 * Selects subjects based on provided set of SubjectNames.
 * Goes over all available subject options.
 */
subject_options([Name|Names], [Subject|Subjects]):-
    subject_option(Name, Subject),
    subject_options(Names, Subjects).

subject_options([], []).

%-------------------------------------------------------dd--
%   Relation definitions
%-------------------------------------------------------dd--

/**
 * subject_is_earlier_without_overlap(+Subject1:subject, +Subject2:subject)
 * 
 * Passes when Subject1 occurs entirely before Subject2.
 */
subject_is_earlier_without_overlap(Subject1, Subject2):-
    subject_interval(Subject1, Interval1),
    subject_interval(Subject2, Interval2),
    earlier_no_overlap(Interval1, Interval2).

/**
 * subject_is_later_without_overlap(+Subject1:subject, +Subject2:subject)
 * 
 * Passes when Subject1 occurs entirely after Subject2.
 */
subject_is_later_without_overlap(Subject1, Subject2):-
    subject_interval(Subject1, Interval1),
    subject_interval(Subject2, Interval2),
    later_no_overlap(Interval1, Interval2).

%-------------------------------------------------------dd--
%   JSON related definitions
%-------------------------------------------------------dd--

:- json_object subject_json(name:string, day:string, from:string, to:string).

/**
 * subject_to_json(+Subject:subject, -JsonSubject:json_object)
 * 
 * Creates matching JSON object representation of Subject in JsonSubject.
 */
subject_to_json(subject(Name, Day, interval(From, To)), JsonSubject):-
    term_string(Name, NameString, [quoted(false)]),
    term_string(Day, DayString, [quoted(false)]),
    time_to_string(From, FromString),
    time_to_string(To, ToString),
    prolog_to_json(subject_json(NameString, DayString, FromString, ToString), JsonSubject).

/**
 * subject_from_json(+DictSubject:dict, -Subject:subject)
 * 
 * Creates matching subject functor from dict object in DictSubject to Subject.
 */
subject_from_json(DictSubject, Subject):-
    time_from_atom(DictSubject.from, From),
    time_from_atom(DictSubject.to, To),
    Subject =.. [subject, DictSubject.name, DictSubject.day, interval(From, To)],
    assertz(Subject),
    debug(general, "Loaded subject: ~w.", [Subject]).
