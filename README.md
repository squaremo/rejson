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
