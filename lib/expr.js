// -*- javascript-indent-level: 2; js-indent-level: 2 -*-

function FAIL() { return false; };
function SUCCEED() { return true; };

function lift_value(value) {
  switch (typeof(value)) {
  case 'function':
    // assume it came from here
    return value;
  case 'object':
    return (is_array(value)) ?
      lift_sequence(value):
      lift_object(value);
  case 'string':
  case 'number':
  case 'boolean':
    return literal(value);
  }
}
module.exports = lift_value;

function match(pattern, json, sk, fk) {
  sk = sk || SUCCEED; fk = fk || FAIL;
  var k = pattern(json, sk, fk);
  // ka-boing!
  while (typeof k === 'function') {
    k = k();
  }
  return k;
}

var base = {
  nullable: function() {
    return false;
  },
  match: function(json, sk, fk) {
    return match(this, json, sk, fk);
  },
  matchInSequence: function(patterns, values, sk, fk) {
    return this(values[0],
                seq_k(patterns.slice(1), values.slice(1), sk, fk),
                fk);
  },
  or: function(rhs) {
    var lhs = this;
    rhs = lift_value(rhs);
    return mixin(function(json, sk, fk) {
      // stack frame
      return lhs(json, sk, function() {
        return rhs(json, sk, fk);
      });
    }, {nullable: function() {
      return lhs.nullable() || rhs.nullable(); }});
  },
};

function mixin(f, extra) {
  extra = extra || {};
  for (k in base) {f[k] = base[k];}
  for (k in extra) {f[k] = extra[k];}
  return f;
}

function ground(type) {
  return mixin(function(json, sk, fk) {
    return (typeof json === type) ? sk : fk;
  });
}
module.exports.ground = ground;
module.exports.number = ground('number');
module.exports.string = ground('string');
module.exports.boolean = ground('boolean');

var is_array = Array.isArray ||
  function (value) {
    return {}.toString.call(value).indexOf('Array') >= 0;
  };

function nullable_seq(patterns) {
  for (var i = 0; i < patterns.length; i++) {
    if (!patterns[i].nullable()) {
      return false;
    }
  }
  return true;
}

function seq_k(patterns, values, sk, fk) {
  return function() {
    if (values.length > 0) {
      if (patterns.length > 0) {
        return patterns[0].matchInSequence(patterns, values, sk, fk);
      }
      else {
        return fk;
      }
    }
    else {
      return nullable_seq(patterns) ? sk : fk;
    }
  }
}

function sequence(patterns) {
  return mixin(function(json, sk, fk) {
    if (is_array(json)) {
      return seq_k(patterns, json, sk, fk);
    }
    else {
      return fk;
    }
  });
}
module.exports.sequence = sequence;

function star(pattern) {
  // Avoid mutating e.g., 'number'
  return mixin(function() { return pattern(v, sk, fk); },
               { matchInSequence:
                 function(patterns, values, sk, fk) {
                   var fk1 = seq_k(patterns.slice(1), values, sk, fk);
                   return pattern(
                     values[0],
                     seq_k(patterns, values.slice(1), sk, fk1),
                     fk1);
                 },
                 nullable: function() { return true; }
               });
}
module.exports.star = star;

function plus(pattern) {
  // plus is "match a value then behave like star".
  var starp = star(pattern);
  return mixin(function (v, sk, fk) { return pattern(v, sk, fk); },
               { matchInSequence:
                 function(patterns, values, sk, fk) {
                   var sk1 = function() {
                     var withstar = patterns.slice(1);
                     withstar.unshift(starp);
                     return seq_k(withstar, values.slice(1), sk, fk);
                   };
                   return pattern(values[0], sk1, fk);
                 }});
}
module.exports.plus = plus;

function object(fields) {
  return mixin(
    function(json, sk, fk) {
      if (typeof json === 'object' && !is_array(json) &&
          Object.keys(json).length == Object.keys(fields).length) {
        // FIXME recursion.
        for (var k in fields) {
          if (!json.hasOwnProperty(k) || !match(fields[k], json[k])) {
            return fk;
          }
        }
        return sk;
      }
      else {
        return fk;
      }
    },
    {
      'etc': function() { return etc(fields); },
    });
}
module.exports.object = object;

function etc(fields) {
  return mixin(function(json, sk, fk) {
    if (typeof json === 'object' && !is_array(json)) {
      // FIXME wildcard, recursion.
      for (var k in fields) {
        if (!json.hasOwnProperty(k) || !match(fields[k], json[k])) {
          return fk;
        }
      }
      return sk;
    }
    else {
      return fk;
    }
  });
}

function literal(value) {
  return mixin(function(json, sk, fk) {
    return (typeof json === typeof value) && (json === value)
      ? sk : fk;
  });
}
module.exports.literal = literal;

function discard() {
  return mixin(function(json, sk) {
    return sk;
  });
}
module.exports.discard = discard;
module.exports.any = discard();

function lift_sequence(values) {
  var patterns = new Array(values.length);
  for (var i=0; i < values.length; i++) {
    patterns[i] = lift_value(values[i]);
  }
  return sequence(patterns);
}

function lift_object(obj) {
  pattern = {};
  for (var k in obj) {
    if (obj.hasOwnProperty(k)) {
      pattern[k] = lift_value(obj[k]);
    }
  }
  return object(pattern);
}
