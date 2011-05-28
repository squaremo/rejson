
Nonterminals capture
             value
             pattern
             variable
             array
             array_head
             maybe_interleave
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
             '=' '{' '}' '[' ']' '*' '+' '?' '^' ',' ':' '|' '(' ')'
.

Rootsymbol capture.
Endsymbol '$end'.

Left 150 '|'.

capture -> variable '=' capture : {capture, '$1', '$3'}.
capture -> variable : {capture, '$1', discard}.
capture -> pattern : '$1'.

pattern -> value : '$1'.
pattern -> discard : discard.
pattern -> ground_type : ground('$1').
pattern -> array : '$1'.
pattern -> object : '$1'.
pattern -> pattern '|' pattern : {either, '$1', '$3'}.
pattern -> '(' capture ')': '$2'.

value -> literal : value('$1').
value -> string : value('$1').

variable -> identifier : variable('$1').

array -> array_head maybe_interleave : interleave('$1', '$2').

maybe_interleave -> '$empty' : [].
maybe_interleave -> '^' array : '$2'.

array_head -> '[' array_pattern ']' : '$2'.

array_pattern -> '$empty' : [].
array_pattern -> array_value array_rest : ['$1' | '$2'].

array_rest -> '$empty' : [].
array_rest -> ',' array_value array_rest : ['$2' | '$3'].

array_value -> pattern : '$1'.
array_value -> variable '=' array_value : {capture, '$1', '$3'}. 
array_value -> pattern '*' : {star, '$1'}.
array_value -> pattern '+' : {plus, '$1'}.
array_value -> pattern '?' : {maybe, '$1'}.

object -> '{' object_pattern '}' : {'$2'}.

object_pattern -> '$empty' : [].
object_pattern -> discard : [discard]. 
object_pattern -> object_value object_rest : ['$1' | '$2'].

object_rest -> '$empty' : [].
object_rest -> ',' discard : [discard].
object_rest -> ',' object_value object_rest : ['$2' | '$3'].

object_value -> string ':' capture : {key('$1'), '$3'}.
object_value -> string ':' capture '?' : {key('$1'), {maybe, '$3'}}.

Erlang code.

ground({ground_type, _Line, Type}) ->
    Type.

value({string, _Line, Chars}) ->
    list_to_binary(string_val(Chars));
value({literal, _Line, Value}) ->
    Value.

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

interleave(Array, []) ->
    Array;
interleave(Seq1, Seq2) ->
    {interleave, Seq1, Seq2}.
