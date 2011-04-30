
Nonterminals document
             value
             pattern
             array
             array_pattern
             array_rest
             array_value
             object
             object_pattern
             object_rest
             object_value
             object_capture
.

Terminals    literal
             string
             ground_type
             variable
             discard
             '=' '{' '}' '[' ']' '*' '+' '?' '^' ',' ':'
.

Rootsymbol document.
Endsymbol '$end'.

document -> object : '$1'.
document -> array : '$1'.

pattern -> value : '$1'.
pattern -> discard : discard.
pattern -> ground_type : '$1'.
pattern -> array : '$1'.
pattern -> object : '$1'.

value -> literal : '$1'.
value -> string : '$1'.

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
array_value -> variable : {capture, '$1'}.
array_value -> variable '=' array_value : {capture, '$1', '$3'}.

object -> '{' object_pattern '}' : {object, '$2'}.

object_pattern -> object_value object_rest : ['$1' | '$2'].

object_rest -> '$empty' : [].
object_rest ->',' object_value object_rest : ['$1' | '$2'].

object_value -> string ':' object_capture : {'$1', '$3'}.
object_value -> discard : discard.

object_capture -> variable '=' pattern : {capture, '$1', '$3'}.
object_capture -> variable : {capture, '$1'}.
object_capture -> pattern : '$1'.
