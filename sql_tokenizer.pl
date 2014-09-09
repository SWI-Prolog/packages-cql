/*  $Id: 6c25708e6981c83dea3b2eca34bc25f90696cd74 $

    Part of SWI-Prolog

    Author:        Matt Lilley
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

    PostgreSQL is a trademark of the PostgreSQL Global Development Group. 
    Microsoft, SQL Server, and Windows are either registered trademarks or
    trademarks of Microsoft Corporation in the United States and/or other
    countries. 
    SQLite is a registered trademark of Hipp, Wyrick & Company, Inc in the United
    States. 
    All other trademarks or registered trademarks are the property of their
    respective owners.

*/

:-module(sql_tokenizer,
         [sql_tokens//1,
          sql_gripe/3]).

sql_gripe(Level, Format, Args):-
        sql_gripe_level(L),
        ( L >= Level ->
            ( L =< 2,
              prolog_load_context(module, Module),
              sql_gripe_exempt_module(Module)->
                true
            ; otherwise->                
                ( prolog_load_context(file, Filename)->
                    true
                ; otherwise->
                    Filename = '<unknown file>'
                ),
                format(atom(Message), Format, Args),
                format(user_error, '~w: ~w~n', [Filename, Message])
            )
        ; otherwise->
            true
        ).
        

% No codes -> no tokens
sql_tokens([], [], []):- !.
sql_tokens([Token|Tokens])-->
        optional_whitespace,
        sql_token(Token),
        optional_whitespace,
        sql_tokens(Tokens).

optional_whitespace-->
        [Code],
        {memberchk(Code, " \t\n\r")}, 
        !,
        optional_whitespace.
optional_whitespace--> [].

:- meta_predicate read_until(//,*,?,?).
read_until(_, [], [], []).
read_until(Terminator, [])-->
        Terminator, !.
read_until(Terminator, [Code|Codes])-->
        [Code],
        read_until(Terminator, Codes).


numeric_codes([Code|Codes])-->
        [Code],
        {memberchk(Code, "0123456789.")},
        !,
        numeric_codes(Codes).
numeric_codes([])--> [], !.

quoted_literal([39|Codes], [39, 39|In], Out):-
        !,
        quoted_literal(Codes, In, Out).
quoted_literal([], [39|In], In):-!.
quoted_literal([Code|Codes])-->
        [Code],
        quoted_literal(Codes).


sql_token(comment(long, Codes))-->
        "/*", !, read_until("*/", Codes).

sql_token(comment(short, Codes))-->
        "--", !, read_until("\n", Codes).

% All of these are a token of their own.
sql_token('=')--> "=", !.
sql_token('<>')--> "!=", !, {sql_gripe(1,'The not-equals operator in SQL is <> and not !=', [])}.
sql_token('<>')--> "! =", !, {sql_gripe(1,'The not-equals operator in SQL is <> and not ! =', [])}.
% There are some WEIRD things that people put in views...
sql_token('>=')--> "! <", !, {sql_gripe(1,'The greater-than-or-equal-to operator in SQL is >= and not ! <', [])}.
sql_token('>=')--> "!<", !, {sql_gripe(1,'The greater-than-or-equal-to operator in SQL is >= and not !<', [])}.
sql_token('<=')--> "!>", !, {sql_gripe(1,'The less-than-or-equal-to operator in SQL is <= and not !>', [])}.
sql_token('<=')--> "! >", !, {sql_gripe(1,'The less-than-or-equal-to operator in SQL is <= and not ! >', [])}.
sql_token('<>')--> "<>", !.
sql_token('>=')--> ">=", !.
sql_token('<=')--> "<=", !.
sql_token('<')--> "<", !.
sql_token('>')--> ">", !.
sql_token('.')--> ".", !.
sql_token(',')--> ",", !.
sql_token(',')--> ",", !.
sql_token('(')--> "(", !.
sql_token(')')--> ")", !.
sql_token('/')--> "/", !.
sql_token('+')--> "+", !.
sql_token('*')--> "*", !.
sql_token('-')--> "-", !.
sql_token('{')--> "{", !.
sql_token('}')--> "}", !.


sql_token(literal(Literal, string))-->
        "'",
        !,
        quoted_literal(Codes),
        {atom_codes(Literal, Codes)}.


sql_token(literal(Literal, identifier))-->
        "\"",
        !,
        read_until("\"", Codes),
        {atom_codes(Literal, Codes)}.

% This should return numeric/2 instead of decimal/2, according to http://msdn.microsoft.com/en-us/library/ms187746
% But it also says they are functionally equivalent
sql_token(literal(Literal, Type))-->
        [Code],
        {memberchk(Code, "0123456789")},
        !,
        numeric_codes(Codes),
        {number_codes(Literal, [Code|Codes])},
        {(integer(Literal)->
            length(Codes, L),
            LL is L+1,
            ( LL > 10->
                Type = decimal(LL, 0)
            ; otherwise->
                Type = int(LL)
            )
         ; Code == 0'0, Codes = [0'.|_]->
            % 0.00 is numeric(2,2) not (3,2). I suppose the leading 0 is just a placeholder?
            length(Codes, L),
            P is L - 1,
            Type = decimal(P, P)
         ; otherwise->
            nth1(P, Codes, 0'.), %'
            
            length(Codes, L),
            S is L - P,
            PP is P + S,
            Type = decimal(PP, S)
         )}.

sql_token(literal(Literal, identifier))-->
        "[",
        !,
        read_until("]", Codes),
        {atom_codes(Literal, Codes)}.

sql_token(Token)-->
        !,
        sql_token_1(Codes),
        {atom_codes(Token, Codes)}.

sql_token_1([], [], []):- !.
% Any of these codes should end the current token. This allows us to correctly
% tokens 3+4 as the sum of two values rather than a single token
sql_token_1([], [Terminator|Codes], [Terminator|Codes]):-
        memberchk(Terminator, ".,()*+-/<=> \t\n\r"), !.

% Everything else goes into the token
sql_token_1([Code|Codes])-->
        [Code],
        sql_token_1(Codes).