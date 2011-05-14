
Nonterminals document
             value
             pattern
             variable
             array
             array_pattern
             array_rest
             array_value
             object
             object_pattern
             object_rest
             object_value
.

Terminals    literal
             string
             ground_type
             identifier
             discard
             '=' '{' '}' '[' ']' '*' '+' '?' '^' ',' ':'
.

Rootsymbol pattern.
Endsymbol '$end'.

pattern -> value : '$1'.
pattern -> discard : discard.
pattern -> ground_type : ground('$1').
pattern -> array : '$1'.
pattern -> object : '$1'.
pattern -> variable : {capture, '$1', discard}.
pattern -> variable '=' pattern : {capture, '$1', '$3'}.

value -> literal : value('$1').
value -> string : value('$1').

variable -> identifier : variable('$1').

array -> '[' array_pattern ']' : {array, '$2'}.

array_pattern -> '$empty' : [].
array_pattern -> array_value array_rest : ['$1' | '$2'].

array_rest -> '$empty' : [].
array_rest -> ',' array_value array_rest : ['$2' | '$3'].

array_value -> pattern : '$1'.
array_value -> pattern '*' : {star, '$1'}.
array_value -> pattern '+' : {plus, '$1'}.
array_value -> pattern '?' : {maybe, '$1'}.
array_value -> array_value '^' array_value : {interleave, '$1', '$3'}.

object -> '{' object_pattern '}' : {object, '$2'}.

object_pattern -> '$empty' : [].
object_pattern -> object_value object_rest : ['$1' | '$2'].

object_rest -> '$empty' : [].
object_rest -> ',' object_value object_rest : ['$2' | '$3'].

object_value -> string ':' pattern : {key('$1'), '$3'}.
object_value -> discard : discard.

Erlang code.

ground({ground_type, _Line, Type}) ->
    {ground, Type}.

value({string, _Line, Chars}) ->
    {value, string_val(Chars)};
value({literal, _Line, Value}) ->
    {value, Value}.

key({string, _Line, Chars}) ->
    string_val(Chars).

string_val(Chars) ->
    unescape(lists:sublist(Chars, 2, length(Chars) - 2)).

variable({identifier, _Line, String}) ->
    String.

unescape(Chars) ->
    unescape1(Chars, []).

unescape1([], Chars) ->
    lists:reverse(Chars);
unescape1([$\\, $\" | R], Chars) ->
    unescape1(R, [$\" | Chars]);
unescape1([C | R], Chars) ->
    unescape1(R, [C | Chars]).
