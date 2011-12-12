var parse = require('../lib/rejson.js').parse;
var assert = require('assert');

function good(desc, input, expected) {
  return test(desc, function() {
    assert.ok(parse(input).match(expected));
  });
}

function bad(desc, input, nomatch) {
  return test(desc, function() {
    assert.ok(!parse(input).match(nomatch));
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
          good("some", "[1, number, string]", [1, -24, "foo"]);
        });
        suite("fail", function() {
          bad("empty v false", "[]", false);
          bad("empty v empty obj", "[]", {});
          bad("empty", "[]", [0]);
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
      });
