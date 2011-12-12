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
