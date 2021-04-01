/**
 * 
 */
:- module(io, [
    %
    load_subjects/2,
    parse_subjects/2,
    %
    print_solutions/1,
    save_solutions/2,
    %
    read_data_json/3,
    write_data_json/2,
    write_data_json/3
]).

% Load local modules.
:- use_module(src/data).
:- use_module(src/logic).

% Load libraries.
:- use_module(library(http/json)).


%===========================================================================dd==
%   SUBJECT RELATED DEFINITIONS
%===========================================================================dd==

/**
 * load_subjects(+File:string, -Subjects:list)
 * 
 * Loads subjects from specified File.
 */
load_subjects(File, Subjects):-
    % open output file
    open(File, read, FileStream, [encoding(utf8)]),
    % write JSON objects to the opened file
    read_data_json(FileStream, JsonSubjects, [value_string_as(atom)]),
    % flush and close the stream
    close(FileStream),
    debug(general, "Subjects loaded from JSON: ~w.", [JsonSubjects]),
    % create internal representations of the subjects
    (
        parse_subjects(JsonSubjects, Subjects);
        print("Failed to load subjects from JSON file.")
    ),
    !,
    debug(general, "Parsed subjects: ~w.", [Subjects]).

/**
 * parse_subjects(+JsonSubjects:list, -Subjects:list)
 * 
 * Parses subject functors from JSON objects in JsonSubjects to Subjects.
 */
parse_subjects([JsonSubject|OtherJsonSubjects], [Subject|OtherSubjects]):-
    subject_from_json(JsonSubject, Subject),
    parse_subjects(OtherJsonSubjects, OtherSubjects).

parse_subjects([], []).


%===========================================================================dd==
%   SOLUTION RELATED DEFINITIONS
%===========================================================================dd==

/**
 * print_solutions(+Solutions:list)
 * 
 * Prints found Solutions to STDOUT.
 */
print_solutions(Solutions):-
    % convert list of solutions to JSON objects
    solutions_to_json(Solutions, JsonSolutions),
    % write JSON objects to STDOUT
    write_data_json(current_output, JsonSolutions).

/**
 * save_solutions(+File:string, +Solutions:list)
 * 
 * Outputs found Solutions to specified File.
 */
save_solutions(File, Solutions):-
    % convert list of solutions to JSON objects
    solutions_to_json(Solutions, JsonSolutions),
    % open output file
    open(File, write, FileStream, [bom(false), encoding(utf8)]),
    % write JSON objects to the opened file
    write_data_json(FileStream, JsonSolutions, [width(0)]),
    % flush and close the stream
    close(FileStream).

%===========================================================================dd==
%   INTERNAL DEFINITIONS
%===========================================================================dd==

%-------------------------------------------------------dd--
%   Input - reads
%-------------------------------------------------------dd--

/**
 * read_data_json(+Stream, +JsonData, +Options:list)
 * 
 * Reads JSON data to JsonData (JSON object(s)) from Stream with Options.
 */
read_data_json(Stream, JsonData, Options):-
    json_read_dict(Stream, JsonData, Options).

%-------------------------------------------------------dd--
%   Output - writes
%-------------------------------------------------------dd--

/**
 * write_data_json(+Stream, +JsonData)
 * 
 * Writes provided JsonData (JSON object(s)) to Stream with Options.
 */
write_data_json(Stream, JsonData):-
    write_data_json(Stream, JsonData, []).

/**
 * write_data_json(+Stream, +JsonData, +Options:list)
 * 
 * Writes provided JsonData (JSON object(s)) to Stream with Options.
 */
write_data_json(Stream, JsonData, Options):-
    json_write(Stream, JsonData, Options).