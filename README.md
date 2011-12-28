# JSON pattern matching

ReJSON is a pattern matching language for JSON, similar in expressive
power to regular expressions for strings, or RelaxNG for XML.

## What does it do?

Here's an example in (the more complete implementation in) Erlang:

    > Json = json:decode("[1, 2, \"foo\", 3]"),
    > Pattern = rejson:parse("[1, 2, number *] ^ [S = string]"),
    > rejson:match(Pattern, Json).
    {ok, [{"S", "foo"}]}

Here's an example in JavaScript (Node.JS or CommonJS). NB this doesn't
implement captures yet, so you can use test to just get `true` or
`false`.

    > var rejson = require('rejson');
    > var pattern = rejson.parse('{"foo": [number *]}');
    > pattern.test({foo: [1, 2]});
    true

You can also lift a JSON-like value into a pattern:

    > var rejson = require('rejson');
    > var pattern = rejson({foo: [rejson.number, rejson.any]});
    > pattern.test({foo: [1, "bar"]});
    true

## Pattern syntax

    <pattern> / <value> => <result>

### Values

All JSON literal values are valid patterns, and match by equivalence.

For example,

    1 / 1 => match

Objects are treated as unordered, so

    {"foo": 1, "bar": 2} / {"bar": 2, "foo": 1} => match

### Ground types

`number`, `string`, and `boolean` each match any value of the
respective type:

    number / 1.5 => match

These can be used nested in an array or object:

    {"foo": number} / {"foo": 10} => match

### Either

A bar signifies alternatives:

    number | string / "foo" => match

### Any

Any, '_', matches any value:

    _ / "foobar" => match

It can be used in arrays and objects:

    [1, 2, 3, _] / [1,2,3,"bar"] => match

    {"foo": _} / {"foo": 23} => match

Used in the place of a whole property it can signify arbitrary
additional properties:

    {"foo": 1, _} / {"foo": 1, "bar": 2} => match

It can only go in the last position for this purpose (object
properties are unordered anyway).

### Star, Plus, Maybe

The Kleene operator '*' (zero or more) and its relative '+' (one or more) can
match in arrays:

    [1, number *] / [1,2,3] => match

Maybe, '?', matches zero or one, and can be used in arrays:

    [number, string ?] / [1] => match
    [number, string ?] / [1, "bar"] => match

In objects, maybe indicates the property may be absent; if it is
present, the pattern must match:

    {"foo": string ?, _} / {"bar": 1} => match
    {"foo": string ?, _} / {"foo": 2} => no_match

### Interleave

Interleave, '^', can be used between array patterns to matches array
values in which the elements matching the left-hand pattern are
arbitrarily interleaved with the elements matching the right-hand
pattern, while staying in order. For example,

    [1, 2, 3] ^ ["foo", "bar"] / [1, "foo", 2, 3, "bar"] => match

### Variable capture

A capture can appear in almost any position, and introduces a pattern
that will prodice a binding if it matches.  For example,

    Foo = number / 3 => match, {Foo: 3}

A capture cannot appear is as a property name, or (currently) as the
operand of an interleave, though it can appear *in* an operand.

## Related work

Here are something things that are like ReJSON:

 - [**Erlang term
    pattern-matching**](http://www.erlang.org/doc/reference_manual/expressions.html#pattern). This
    is pretty close, but it doesn't deal with matching objects (maps),
    or sequences (repetition and interleave).

 - [**CDuce pattern matching and
 types**](http://www.cduce.org/manual_types_patterns.html). This is
 probably closest in spirit, even though it is based on XML
 schema. CDuce goes further and has a type system built on these
 patterns.

 - [**JSON schema**](http://json-schema.org/). This isn't
    pattern-matching, but it is close in that it's verifying the shape
    of a JSON term. However, JSON schema is unsurprisingly more akin
    to XML Schema, in that it encodes a language entirely unlike JSON,
    but describing JSON, in JSON.

    I think it's better to make the pattern language parallel to the
    value language, even if it can't reuse the parser. I'm also trying
    to make a pattern-matcher, rather than a schema-checker. In other
    words, the idea is to get variable bindings out the other end, not
    only see if a term matches some shape.

 - *Other attempts*, notably
    [jsonr](http://laurentszyster.be/jsonr/). This is more
    text-oriented, with no binding capture, but is close in spirit to
    rejson. It's limited by insisting on being encoded in JSON.

## Further work

### Matching with a streaming parser

It's fairly obvious that parsing the entire JSON value ahead of time
is a waste, if the match fails early on. Using a stream parser (or
just a lazy tokeniser, JSON is simple enough) would mean only doing
the parsing that's needed.

Backtracks would need to save the state of the parser; but of course,
in a functional language this isn't a big deal.

### Determining which of a set of patterns matches a value

Often the problem at hand is not "which values match this pattern?",
but "which patterns does this value match?". One way to do this is to
compile all patterns into a state machine where the terminal states
yield a list of patterns matched -- something like
http://en.wikipedia.org/wiki/Aho-Corasick_algorithm.

I can foresee two main difficulties: variable captures and
interleave. Interleave because it results in state explosion. Variable
capture because it's just awkward.
