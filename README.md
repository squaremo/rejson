# JSON pattern matching

This is an experiment to see if I can come up with a pattern-matching
language for JSON. It may lead to a full unifier, who knows.

## What does it do?

Not much, yet, just parsing pattern strings. I'll implement the actual
matcher in good time ..

What will it do (if I finish it)? Here's a suggestive example:

     > Json = json:decode("[1, 2, \"foo\", 3]"),
     > Pattern = rejson:parse("[1, 2, number *] ^ [S = string]"),
     > rejson:match(Pattern, Json).
     {match, [{"S", "foo"}]}

## Related work

Here are something things that are like rejson:

 - *Erlang term pattern-matching*. Actually this is pretty close, but
    it doesn't deal with matching objects (maps), or sequences
    (repetition and interleave).

 - *JSON schema*. This isn't pattern-matching, but it is close in that
    it's verifying the shape of a JSON term. However, JSON schema is
    unsurprisingly more akin to XML Schema, in that it encodes a
    language entirely unlike JSON, but describing JSON, in JSON.

    I think it's better to make the pattern language parallel to the
    value language, even if it can't reuse the parser.

    I'm also trying to make a pattern-matcher, rather than a
    schema-checker. In other words, the idea is to get variable
    bindings out the other end, not only see if a term matches some
    shape.

 - *Other attempts*, notably
    [jsonr](http://laurentszyster.be/jsonr/). This is more
    text-oriented, with no binding capture, but is close in spirit to
    rejson. It's limited by insisting on being encoded in JSON.

## Pattern syntax

### Values
   
All JSON literal values are valid patterns, and match by equivalence.

For example,

    1 / 1 |- match

and

    {"foo": 1, "bar": 2} / {"bar": 2, "foo": 1} |- {match, []}

### Ground types

`number`, `string`, and `boolean` each match any value of the
respective type:

    number / 1.5 |- match

These can be used nested in an array or object:

    {"foo": number} / {"foo": 10} |- match

### Any

Any, '_', matches any value:

    _ / "foobar" |- match

It can be used in arrays and objects:

    [1, 2, 3, _] / [1,2,3,"bar"] |- match

    {"foo": _} / {"foo": 23} |- match

Used in the place of a whole property it can signify arbitrary
additional properties:

    {"foo": 1, _} / {"foo": 1, "bar": 2} |- match

It can only go in the last position for this purpose (object
properties are unordered anyway).

### Star, Plus, Maybe

The Kleene operator '*' (zero or more) and its relative '+' (one or more) can
match arrays:

    [number *] / [1,2,3] |- match

Maybe, '?', matches zero or one, and can be used in arrays or objects:

    [number, string ?] / [1] |- match
    [number, string ?] / [1, "bar"] |- match

    {"foo": string ?, _} / {"bar": 1} |- match
    {"foo": string ?, _} / {"foo": 2} |- match

### Interleave

Interleave, '^', can be used between array patterns to matches array values
in which the elements matching the left-hand pattern are arbitrarily
interleaved with the elements matching the right-hand pattern. For
example,

    [1, 2, 3] ^ ["foo", "bar"] / [1, "foo", 2, 3, "bar"] |- match

### Variable capture

A capture can appear in almost any position, and introduces a pattern
that will prodice a binding if it matches.  For example,

    Foo = number / 3 |- match, Foo = 3

The one place a capture cannot appear is as a property name.
