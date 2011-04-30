
Definitions.

Rules.

\{ : {token, {'{', TokenLine}}.
\} : {token, {'}', TokenLine}}.
\[ : {token, {'[', TokenLine}}.
\] : {token, {']', TokenLine}}.
\= : {token, {'=', TokenLine}}.
\* : {token, {'*', TokenLine}}.
\+ : {token, {'+', TokenLine}}.
\? : {token, {'?', TokenLine}}.
\^ : {token, {'^', TokenLine}}.
\, : {token, {',', TokenLine}}.
\: : {token, {':', TokenLine}}.

\"([^\"]|(\"))*\" : {token, {string, TokenLine, TokenChars}}.

(\+|-)?[0-9]+\.[0-9]+((E|e)(\+|-)?[0-9]+)? :
  {token, {literal, TokenLine, list_to_float(TokenChars)}}.

(\+|-)?[0-9]+ :
  {token, {literal, TokenLine, list_to_integer(TokenChars)}}.

(number)|(string)|(boolean) :
  {token, {ground_type, TokenLine, list_to_atom(TokenChars)}}.

(true)|(false)|(null) :
  {token, {literal, TokenLine, list_to_atom(TokenChars)}}.

_?[a-zA-Z_]+ : {token, {variable, TokenLine, TokenChars}}.

_ : {token, {discard, TokenLine}}.

[\v\t\s\r\l] : skip_token.

Erlang code.
