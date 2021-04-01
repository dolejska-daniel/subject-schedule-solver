/**
 * 
 */
:- module(io, [
    %
    print_solutions/1,
    save_solutions/2
]).

% Load local modules.
:- use_module(data).
:- use_module(logic).

% Load libraries.
:- use_module(library(http/json)).

/**
 * print_solutions(+Solutions:list)
 * 
 * Prints found Solutions to STDOUT.
 */
print_solutions(Solutions):-
    % convert list of solutions to JSON objects
    solutions_to_json(Solutions, JsonSolutions),
    % write JSON objects to STDOUT
    write_solutions_json(current_output, JsonSolutions).

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
    write_solutions_json(FileStream, JsonSolutions, [width(0)]),
    % flush and close the stream
    close(FileStream).

%===========================================================================dd==
%   INTERNAL DEFINITIONS
%===========================================================================dd==

/**
 * write_solutions_json(+Stream, +JsonSolutions:list)
 * 
 * Outputs found JsonSolutions (list of solutions already as JSON objects) to Stream.
 */
write_solutions_json(Stream, JsonSolutions):-
    write_solutions_json(Stream, JsonSolutions, []).

/**
 * write_solutions_json(+Stream, +JsonSolutions:list, +Options:list)
 * 
 * Outputs found JsonSolutions (list of solutions already as JSON objects) to Stream with Options.
 */
write_solutions_json(Stream, JsonSolutions, Options):-
    json_write(Stream, JsonSolutions, Options).