/**
 * 
 */
:- module(utils, [
    arg_value/3,
    arg_value_atom/3
]).


%===========================================================================dd==
%   CLI ARGUMENT RELATED DEFINITIONS
%===========================================================================dd==

/**
 * arg_value(+ArgName:string, +Default, -ArgValue)
 * 
 * Gets value ArgValue of ArgName CLI argument defaulting to Default.
 */
arg_value(ArgName, Default, ArgValue):-
    current_prolog_flag(argv, Args),
    arg_value(ArgName, Args, Default, ArgValue).

arg_value(ArgName, [Arg|Args], Default, Value):-
    (
        arg_match(ArgName, Arg, Value);
        arg_value(ArgName, Args, Default, Value)
    ).

arg_value(_, [], Default, Default).

/**
 * arg_value_atom(+ArgName:string, +Default, -ArgValue)
 * 
 * Gets value ArgValue of ArgName CLI argument defaulting to Default and converting the value to atom.
 */
arg_value_atom(ArgName, Default, Value):-
    arg_value(ArgName, Default, InitialValue),
    string_to_atom(InitialValue, Value).

/**
 * arg_match(+ArgName:string, +Arg:string, -ArgValue)
 * 
 * Finds CLI argument of name ArgName and its value ArgValue in Arg if present.
 */
arg_match(ArgName, Arg, ArgValue):-
    % split string with "="
    sub_string(Arg, Before, _, After, "="), !,
    % unify prefix with argument name
    sub_string(Arg, 0, Before, _, ArgName),
    % extract argument value from suffix
    sub_string(Arg, _, After, 0, ArgValue).
