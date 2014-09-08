/*  $Id: 27f48e70699a3e00633cad1ab7b98c14818f2891 $

    Part of SWI-Prolog

    Author:        Mike Elston
                   Matt Lilley
    E-mail:        matt.s.lilley@gmail.com
    WWW:           http://www.swi-prolog.org
    Copyright (C): 1985-2014, Mike Elston, Matt Lilley

    This program is free software; you can redistribute it and/or
    modify it under the terms of the GNU General Public License
    as published by the Free Software Foundation; either version 2
    of the License, or (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA

    As a special exception, if you link this library with other files,
    compiled with a Free Software compiler, to produce an executable, this
    library does not by itself cause the resulting executable to be covered
    by the GNU General Public License. This exception does not however
    invalidate any other reasons why the executable file might be covered by
    the GNU General Public License.

    PostgreSQL is a trademark of the PostgreSQL Global Development Group
    Microsoft, SQL Server, and Windows are either registered trademarks or
    trademarks of Microsoft Corporation in the United States and/or other
    countries.
    SQLite is a registered trademark of Hipp, Wyrick & Company, Inc.

*/

% tar -cjvf cql.tar.bz2 cql.pl sql_parser.pl sql_tokenizer.pl sql_write.pl sql_keywords.pl cql_database.pl cql_api.pl cql_hooks.pl cql_package.pl cql_demo.pl

:-use_module(library(chr)).
:-use_module(library(dcg/basics)).

chr_meta_predicate(_).
:-op(400, xfy, (::)).            % CQL and standard_form
:-op(900, fy,  exists).          % CQL
:-op(750, yfx, *==).             % CQL
:-op(750, yfx, =*=).             % CQL
:-op(750, yfx, ==*).             % CQL
:-op(740, yfx, on).              % CQL
:-op(700, xfx, =~).              % CQL (LIKE)
:-op(700, xfx, \=~).             % CQL (NOT LIKE)
:-op(200, fy, #).                % CQL (nolock)
:-op(920, fy, ???).              % Debugging
:-op(920, fy, ??).               % Debugging
:-op(920, fy, ?).                % Debugging

duplicates(List, SortedDuplicates):-
        msort(List, SortedList),
        duplicates(SortedList, [], Duplicates),
        sort(Duplicates, SortedDuplicates).

duplicates([], Duplicates, Duplicates) :- !.
duplicates([_], Duplicates, Duplicates) :- !.
duplicates([A, B|T1], Duplicates, T2) :-
        A == B,
        !,
        ( memberchk(A, Duplicates) ->
            duplicates(T1, Duplicates, T2)
        ; otherwise->
            duplicates(T1, [A|Duplicates], T2)
        ).
duplicates([_|T1], Duplicates, T2) :-
        duplicates(T1, Duplicates, T2).

map_database_atom(Keyword, Mapped):-
        reserved_sql_keyword(Keyword), !,
        format(atom(Mapped), '"~w"', [Keyword]).
map_database_atom(Atom, Atom).

strip_sort_keys([], []).
strip_sort_keys([_-Detail|T1], [Detail|T2]) :-
        strip_sort_keys(T1, T2).

path_arg([], Term, Term).
path_arg([Index|Indices], Term, SubTerm) :-
        compound(Term),
	arg(Index, Term, Arg),
	path_arg(Indices, Arg, SubTerm).



:-ensure_loaded(cql).
:-ensure_loaded(sql_parser).
:-ensure_loaded(sql_tokenizer).
:-ensure_loaded(sql_write).
:-ensure_loaded(sql_keywords).
:-ensure_loaded(cql_database).
:-ensure_loaded(cql_api).
:-ensure_loaded(cql_se).


user:goal_expansion(Schema:{Cql}, GoalExpansion) :-
        atom(Schema),
        !,
        cql_goal_expansion(Schema, Cql, GoalExpansion).

user:goal_expansion({Cql}, GoalExpansion) :- !,
        default_schema(Schema),
        cql_goal_expansion(Schema, Cql, GoalExpansion).

user:goal_expansion(?(Goal), cql_show(Goal, minimal)):-
        nonvar(Goal),
        ( Goal = {_} ; Goal = _:{_} ).
user:goal_expansion(??(Goal), cql_show(Goal, explicit)):-
        nonvar(Goal),
        ( Goal = {_} ; Goal = _:{_} ).
user:goal_expansion(???(Goal), cql_show(Goal, full)):-
        nonvar(Goal),
        ( Goal = {_} ; Goal = _:{_} ).



% SE Cruft
advise(_, _, Format, Args):-
        debug(cql(logging), Format, Args).
accurate_wall_clock_time(T):- get_time(T).
throw_exception(ErrorId, Format, Args):-
        format(atom(Message), Format, Args),
        throw(cql_error(ErrorId, Message)).
throw_exception(ErrorId, Message):-
        throw(cql_error(ErrorId, Message)).

atom_to_rational(Atom, Rational):-
        atom_to_term(Atom, X, _),
        Rational is rationalize(X).
show_debug_output(Format, Args):-
        format(Format, Args).
console(Format, Args):-
        format(Format, Args).
event_log(_, Format, Args):-
        format(Format, Args).

odbc_execute_with_statistics(Statement, OdbcParameters, _OdbcParameterDataTypes, Row):-
        odbc_execute(Statement, OdbcParameters, Row).

%%      attribute_domain(+Schema, +TableName, +ColumnName, -Domain).
attribute_domain(Schema, TableName, ColumnName, Domain):-
        odbc_data_type(Schema, TableName, ColumnName, Domain).

%%      database_identity(?Schema:atom, ?EntityName:atom, ?ColumnName:atom)
database_identity(Schema, EntityName, ColumnName) :-
        database_attribute(_, Schema, EntityName, ColumnName, _, _, is_identity(true), _).

domain_database_data_type(Domain, Type) :-
        checked_domain(Domain, _, _, _, _, _, Type).


%%      database_key(?Schema:atom, ?EntityName:atom, ?ConstraintName:atom, ?KeyColumnNames:list, ?KeyType)
%
%       @param KeyColumnNames list of _|atom|_ in database-supplied order
%       @param KeyType _|identity|_ ; _|'primary key'|_ ; _|unique|_

database_key(Schema, EntityName, ConstraintName, KeyColumnNames, 'primary key') :-
        database_constraint(Schema, EntityName, ConstraintName, primary_key(KeyColumnNames)).

database_key(Schema, EntityName, ConstraintName, KeyColumnNames, unique) :-
        database_constraint(Schema, EntityName, ConstraintName, unique(KeyColumnNames)).
        
database_key(Schema, EntityName, identity, [ColumnName], identity) :-
        database_identity(Schema, EntityName, ColumnName).

checked_domain(Domain, _, _, _, _, _, DataType):-
        database_domain(Domain, DataType).



get_data_type(Schema,                                          % +
              TableSpec,                                       % +
              ColumnName,                                      % ?
              DatabaseDataType,                                % ?
              CharacterMaximumLength,                          % ?
              NumericPrecision,                                % ?
              NumericScale,                                    % ?
              Domain,                                          % ?
              OrdinalPosition,                                 % ?
              Nullability) :-
        cached_data_type(Schema,
                           TableSpec,
                           ColumnName,
                           DatabaseDataType,
                           CharacterMaximumLength,
                           NumericPrecision,
                           NumericScale,
                           Domain,
                           OrdinalPosition,
                           Nullability,
                           _,    % IsIdentity
                           _).
        
cached_data_type(Schema, EntityName, ColumnName, DatabaseDataType, MaximumLength, NumericPrecision, NumericScale, Domain, _, boolean(AllowsNullsTrueFalse), boolean(IsIdentityTrueFalse), ColumnDefault) :-
        database_attribute(_, Schema, EntityName, ColumnName, domain(Domain), allows_nulls(AllowsNullsTrueFalse), is_identity(IsIdentityTrueFalse), ColumnDefault),
        checked_domain(Domain, _, _, _, _, _, DataType),
        data_type_length_precision_scale(DataType, DatabaseDataType, MaximumLength, NumericPrecision, NumericScale).

cached_data_type(Schema, EntityName, ColumnName, DatabaseDataType, MaximumLength, NumericPrecision, NumericScale, {null}, _, boolean(AllowsNullsTrueFalse), boolean(IsIdentityTrueFalse), ColumnDefault) :-
        database_attribute(_, Schema, EntityName, ColumnName, native_type(DataType), allows_nulls(AllowsNullsTrueFalse), is_identity(IsIdentityTrueFalse), ColumnDefault),
        data_type_length_precision_scale(DataType, DatabaseDataType, MaximumLength, NumericPrecision, NumericScale).

data_type_length_precision_scale(DataType, DatabaseDataType, MaximumLength, NumericPrecision, NumericScale) :-
        once(data_type_length_precision_scale_1(DataType, DatabaseDataType, MaximumLength, NumericPrecision, NumericScale)).

data_type_length_precision_scale_1(varchar(MaximumLength), varchar, MaximumLength, {null}, {null}).
data_type_length_precision_scale_1(nvarchar(MaximumLength), nvarchar, MaximumLength, {null}, {null}).
data_type_length_precision_scale_1(varbinary(MaximumLength), varbinary, MaximumLength, {null}, {null}).
data_type_length_precision_scale_1(decimal(NumericPrecision, NumericScale), decimal, {null}, NumericPrecision, NumericScale).
data_type_length_precision_scale_1(int, integer, 10, {null}, {null}).
data_type_length_precision_scale_1(DataType, DataType, {null}, {null}, {null}).


port_label(unify,              'CALL  ', green).
port_label(call,               'CALL  ', cyan).
port_label(pending,            'EXIT  ', yellow).
port_label(exit,               'EXIT  ', white).
port_label(!,                  'EXIT !', white).
port_label(fail,               'FAIL  ', magenta).
port_label(exception,          'ERROR ', red).
port_label(external_exception, 'ERROR ', red).

get_host_name(X):- gethostname(X).

% FIXME: These should all not be necessary!
sql_gripe_exempt_module(_).
t7_to_unambiguous_atom(t7(Y, M, D, H, Min, S, Ms), Atom):-
        format(atom(Atom), '~`0t~w~4+-~`0t~w~3+-~`0t~w~3+ ~`0t~w~3+:~`0t~w~3+:~`0t~w~3+.~`0t~w~4+', [Y, M, D, H, Min, S, Ms]).

primary_schema(X):- default_schema(X).
compiling_from_makefile:- fail.
domain_allowed_value(_, _).
entity_name_module_file_base_name(_, _, _).
event_notification_table(_, _):- fail.
history_attribute(_, _, _):- fail.
suggest_indices(_).
is_a(Var,var):- var(Var), !.
is_a(Atom,atom):- atom(Atom), !.
is_a(Atom,integer):- integer(Atom), !.
is_a(Atom,rational):- rational(Atom), !.
is_a(Atom,t7):- \+var(Atom), Atom = t7(_,_,_,_,_,_,_), !.
is_a(Atom,boolean):- \+var(Atom), Atom = boolean(X), (X == true ; X == false), !.
is_a(Atom, null):- \+var(Atom), Atom == {null}.

catch_all(A, B, C):- catch(A, B, C).
t7_now(t7(Y, M, D, HH, MM, SS, NN)):-
        get_time(X),
        stamp_date_time(X, date(Y,M,D,HH,MM,S,_,_,_), local),
        NN is round(float_fractional_part(S) * 1000),
        SS is integer(float_integer_part(S)).

process_database_events(_).

??(Goal):-
        setup_call_catcher_cleanup(format('CALL  ~q~n', [Goal]),
                                   Goal,
                                   Catcher,
                                   ( Catcher == ! ->
                                       format('CUT   ~q~n', [Goal])
                                   ; Catcher == fail->
                                       format('FAIL  ~q~n', [Goal])
                                   ; Catcher == exit->
                                       format('EXIT  ~q~n', [Goal])
                                   ; Catcher = error(E)->
                                       format('ERROR  ~q~n~w~n', [E])
                                   )),
        ( var(Catcher)->
            format('PEND  ~q~n', [Goal])
        ; otherwise->
            true
        ).
