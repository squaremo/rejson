// -*- javascript-indent-level: 2; js-indent-level: 2 -*-

var parse = require('../lib/rejson.js').parse;
var assert = require('assert');

function good(desc, input, expected) {
  return test(desc, function() {
    assert.ok(parse(input).test(expected));
  });
}

function bad(desc, input, nomatch) {
  return test(desc, function() {
    assert.ok(!parse(input).test(nomatch));
  });
}

function nonsense(desc, input) {
  return test(desc, function() {
    assert.throws(function() {parse(input);});
  });
}

suite("Primitives",
      function() {
        suite("succeed", function() {
          good("discard v number", "_", 2345);
          good("discard v string", "_", "foobar");
          good("discard v array", "_", [1, 2, 3]);
          good("discard v obj", "_", {foo: 45, bar: "foobar"});
          good("discard v null", "_", null);
          good("null", "null", null);
          good("a number", "34", 34);
          good("negative number", "-781", -781);
          good("a float", "-1.0", -1.0);
          good("scientific notation", "1.0e+2", 100.0);
          good("a string", '"foobar"', "foobar");
          good("a boolean", 'false', false);
        });
        suite("fail", function() {
          bad("null v false", "null", false);
          bad("zero v false", "0", false);
          bad("empty string v false", '""', false);
        });
        suite("nonsense", function() {
          nonsense("Unbalanced quotes", '"foo');
          nonsense("Not a number", '1.0.0');
        })
      });

suite("Ground types",
      function() {
        suite("succeed", function() {
          good("string", "string", "foo");
          good("string v empty string", "string", "");
          good("number", "number", 91);
          good("number v negative number", "number", -15.2);
          good("number v scientific notation", "number", 0.72E-6);
        });
        suite("fail", function() {
          bad("string v number", "string", 3256);
          bad("string v boolean", "string", false);
          bad("number v numbery string", "number", "914");
          bad("number v boolean", "number", true);
        });
      });

suite("Arrays",
      function() {
        suite("succeed", function() {
          good("empty", "[]", []);
          good("single", "[72]", [72]);
          good("nested", "[[number]]", [[-2467.5]]);
          good("some", "[_, number, string]", [1, -24, "foo"]);
        });
        suite("fail", function() {
          bad("empty v false", "[]", false);
          bad("empty v empty obj", "[]", {});
          bad("empty", "[]", [0]);
          bad("discard v empty", "[_]", []);
        });
        suite("nonsense", function() {
          nonsense("mismatch", "[1, 4");
          nonsense("missing comma", "[number number]");
        });
      });

suite("Objects",
      function() {
        suite("succeed", function() {
          good("empty", "{}", {});
          good("single", '{"foo": number}', {foo: 65});
          good("ordered", '{"foo": string, "bar": number}',
               {foo: "foo", bar: -17});
          good("unordered", '{"bar": number, "foo": string}',
               {foo: "foo", bar: 71});
          good("any obj", '{_}', {foo: 4, bar: "bar"});
          good("subset", '{"foo": 26, _}', {bar: 56, foo: 26});
        });
        suite("fail", function() {
          bad("empty v non-empty", "{}", {foo: false});
          bad("empty v false", "{}", false);
          bad("empty v true", "{}", true);
          bad("subset", '{"foo": number}', {foo: 7, bar: 8});
          bad("superset", '{"foo": number, "bar": number}', {foo: 8});
          bad("value mismatch", '{"foo": string}', {foo: -24});
        });
        suite("nonsense", function() {
          nonsense("mismatch", '{"foo": number"');
          nonsense("no key", '{number}');
          nonsense("no key again", '{"foo": _, string}');
          nonsense("discard in middle", '{"foo": number, _, "bar": string}');
        });
      });

suite("Or",
      function() {
        suite("succeed", function() {
          good("ground types", "number | string", 4);
          good("ground types", "number | string", "foo");
          good("in array", "[_, number | string]", [false, "foo"]);
          good("in obj", '{"bar": string | false}', {bar: "barfoo"});
          good("associate", "number | boolean | string", "foo");
        });
        suite("fail", function() {
          bad("not included", "number | string", true);
        });
        suite("nonsense", function() {
          nonsense("no LHS", "| number");
          nonsense("no RHS", "string |");
        });
      });

suite("Repeat",
      function() {
        suite("succeed", function() {
          good("star", "[number *]", [1, 2, 3]);
          good("star matching empty", "[number *]", []);
          good("star backtracking", "[number *, number]", [1]);
          good("star times two", "[number *, number *]", [1, 2, 3]);
          good("star different kinds", "[number *, string *]",
               [1, 2, "foo", "bar"]);

          good("plus", "[number +]", [1, 2, 3]);
        });
        suite("fail", function() {
          bad("wrong type", "[number *]", ["string"]);
          bad("at least one", "[number, number *]", []);
          bad("at least one for plus", "[number +]", []);
        });
        suite("nonsense", function() {
          nonsense("two stars", "[number **]");
          nonsense("wrong place", "[* number]");
          nonsense("no argument", "[number, *]");
        });
      });

suite("Interleave",
      function() {
        suite("succeed", function() {
          good("empty", "[] ^ []", []);
          good("lhs empty", "[] ^ [1, 2, 3]", [1, 2, 3]);
          good("rhs empty", "[1, 2, 3] ^ []", [1, 2, 3]);
          good("alt pairs", "[1, 2] ^ [3, 4]", [1, 3, 2, 4]);
          good("in order", "[1, 2] ^ [3, 4]", [1, 2, 3, 4]);
          good("discard", "[1, 2] ^ [_]", [1, 3, 2]);
          good("with star", "[number *] ^ [1, 2]", [6, 1, 7, 3, 2]);
          good("with star rev", "[1, 2] ^ [_ *]", [1, 3, 2]);
          good("with or", "[1, 2] ^ [(number|string)*]", ["foo", 1, 3, 2]);
          good("with plus", "[1, string +] ^ [2, 3]", [2, 1, 3, "foo", "bar"]);
          good("with plus rev", "[2, 3] ^ [1, string +]",
               [2, 1, 3, "foo", "bar"]);
        });
        suite("fail", function() {
          bad("empty v not", "[] ^ []", [1]);
          bad("out of order", "[1, 2] ^ [3, 4]", [1, 2, 4, 3]);
          bad("out of order alt", "[1, 2] ^ [3, 4]", [1, 4, 2, 3]);
          bad("missing element", "[number *] ^ [1, 2]", [3, 1, 6, 7]);
          good("with plus", "[1, string +] ^ [2, 3]", [2, 1, 3]);
          good("with plus rev", "[2, 3] ^ [1, string +]", [2, 1, 3]);
        });
        suite("nonsense", function() {
          nonsense("no lhs", '^ [1, 2]');
          nonsense("no rhs", '[1, 2] ^');
        });
      });
