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
  matchInSequence: function(patterns, values, sk, fk, p, v) {
    return patterns[p](values[v],
                       seq(patterns, values, sk, fk,
                           p + 1, v + 1), fk);
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

function nullable_seq(patterns, ind) {
  var len = patterns.length;
  var after = (patterns.hasOwnProperty('nullableAfter')) ?
    patterns.nullableAfter : patterns.length;
  if (ind >= after) {
    return true;
  }
  else {
    // If we did this backwards, we'd advance after more often,
    // but exit the loop later each time. Assumption: star
    // will mostly tend to appear last.
    for (var i = ind; i < after; i++) {
      if (!patterns[i].nullable()) {
        return false;
      }
    }
    patterns.nullableAfter = ind;
    return true;
  }
}

function seq(patterns, values, sk, fk, p, v) {
  return function() {
    if (v < values.length) {
      if (p < patterns.length) {
        return patterns[p].matchInSequence(patterns, values, sk, fk, p, v);
      }
      else {
        return fk;
      }
    }
    else {
      return nullable_seq(patterns, p) ? sk : fk;
    }
  }
}

function sequence(patterns) {
  return mixin(function(json, sk, fk) {
    if (is_array(json)) {
      return seq(patterns, json, sk, fk, 0, 0);
    }
    else {
      return fk;
    }
  });
}
module.exports.sequence = sequence;

function star(pattern) {
  return mixin(pattern,
               { matchInSequence:
                 function(patterns, values, sk, fk, p, v) {
                   var fk1 = seq(patterns, values, sk, fk, p + 1, v);
                   return patterns[p](
                     values[v],
                     seq(patterns, values, sk, fk1, p, v + 1),
                     fk1)
                 },
                 nullable: function() { return true; }
               });
}
module.exports.star = star;

function plus(pattern) {
  return pattern;
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
