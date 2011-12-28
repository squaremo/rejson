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

function run_match(pattern, json, sk, fk) {
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
    return run_match(this, json, sk, fk);
  },
  test: function(json, sk, fk) {
    return this.match(json, sk, fk);
  },
  matchInSequence: function(patterns, values, sk, fk) {
    return this(values[0],
                seq_k(patterns.slice(1), values.slice(1), sk, fk),
                fk);
  },
  matchInInterleave: function(seq1, seq2, values, sk, fk) {
    return this(values[0],
                intr_k(seq1.slice(1), seq2, values.slice(1), sk, fk),
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
    }, {
      nullable: function() { return lhs.nullable() || rhs.nullable(); },
      toString: function(paren) {
        return paren_str(lhs.toString(true) + '|' + rhs.toString(true), paren);
      }
    });
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
  }, {
    'toString': function() { return type; }
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

function paren_str(expr, paren) {
  return (paren) ? '(' + expr + ')' : expr;
}

function seq_str(patterns) {
  var res = [];
  patterns.forEach(function(p) { res.push(p.toString()); });
  return '[' + res.join(',') + ']';
}

function sequence(patterns) {
  return mixin(function(json, sk, fk) {
    if (is_array(json)) {
      return seq_k(patterns, json, sk, fk);
    }
    else {
      return fk;
    }
  }, {
    'toString': function() {
      return seq_str(patterns);
    }
  });
}
module.exports.sequence = sequence;

function star(pattern) {
  // Avoid mutating e.g., 'number'
  return mixin(function(v, sk, fk) { return pattern(v, sk, fk); },
               { matchInSequence:
                 function(patterns, values, sk, fk) {
                   var fk1 = seq_k(patterns.slice(1), values, sk, fk);
                   return pattern(
                     values[0],
                     seq_k(patterns, values.slice(1), sk, fk1),
                     fk1);
                 },
                 matchInInterleave:
                 function(seq1, seq2, values, sk, fk) {
                   return pattern(
                     values[0],
                     intr_k(seq1, seq2, values.slice(1), sk, fk),
                     intr_k(seq1.slice(1), seq2, values, sk, fk));
                 },
                 nullable: function() { return true; },
                 toString: function() { return pattern.toString(true) + ' *';}
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
                 },
                 matchInInterleave:
                 function(seq1, seq2, values, sk, fk) {
                   var withstar = seq1.slice(1);
                   withstar.unshift(starp);
                   return pattern(
                     values[0],
                     intr_k(withstar, seq2, values.slice(1), sk, fk),
                     intr_k(seq1.slice(1), seq2, values, sk, fk));
                 },
                 toString: function() { return pattern.toString(true); }
               });
}
module.exports.plus = plus;

function interleave(seq1, seq2) {
  return mixin(function(json, sk, fk) {
    if (is_array(json)) {
      return intr_k(seq1, seq2, json, sk, fk);
    }
    else {
      return fk;
    }
  }, {
    nullable: function() {
      return nullable_seq(seq1) && nullable_seq(seq2);
    },
    toString: function(paren) {
      return paren_str(seq_str(seq1) + '^' + seq_str(seq2), paren);
    }
  });
}
module.exports.interleave = interleave;

function intr_k(seq1, seq2, json, sk, fk) {
  if (seq1.length === 0) {
    return function() {
      //console.log("seq1 is zero, resort to " + seq_str(seq2));
      return seq_k(seq2, json, sk, fk);
    }
  }
  else if (seq2.length === 0) {
    return function() {
      //console.log("seq2 is zero, resort to " + seq_str(seq1));
      return seq_k(seq1, json, sk, fk);
    }
  }
  else if (json.length === 0) {
    return (nullable_seq(seq1) && nullable_seq(seq2)) ? sk : fk;
  }
  else {
    return function() {
      //console.log("trying " + seq_str(seq1) + '^' + seq_str(seq2) +
      // '\\' + seq_str(json));
      var head1 = seq1[0];
      var head2 = seq2[0];
      return head1.matchInInterleave(
        seq1, seq2, json,
        sk, function() {
          //console.log("fail with " +
          // seq_str(seq1)+'^'+seq_str(seq2)+'\\' + seq_str(json) +
          // ", " + "try other way around");
          return head2.matchInInterleave(
            seq2, seq1, json, sk, fk);
        });
    }
  }
}

function object(fields) {
  return mixin(
    function(json, sk, fk) {
      if (typeof json === 'object' && !is_array(json) &&
          Object.keys(json).length == Object.keys(fields).length) {
        // FIXME recursion.
        for (var k in fields) {
          if (!json.hasOwnProperty(k) || !fields[k].test(json[k])) {
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
      etc: function() { return etc(fields); },
      toString: function() {
        return '{' + fields_str(fields) + '}';
      }
    });
}
module.exports.object = object;

function fields_str(fields) {
  var res = [];
  for (var k in fields) {
    res.push('"' + k + '": ' + fields[k].toString());
  }
  return res.join(',');
}

function etc(fields) {
  return mixin(function(json, sk, fk) {
    if (typeof json === 'object' && !is_array(json)) {
      // FIXME wildcard, recursion.
      for (var k in fields) {
        if (!json.hasOwnProperty(k) || !fields[k].test(json[k])) {
          return fk;
        }
      }
      return sk;
    }
    else {
      return fk;
    }
  }, {
    toString: function() {
      return '{' + fields_str(fields) + ', _}';
    }
  });
}

function literal(value) {
  return mixin(function(json, sk, fk) {
    return (typeof json === typeof value) && (json === value)
      ? sk : fk;
  }, {
    toString: function() { return value.toString(); }
  });
}
module.exports.literal = literal;

function discard() {
  return mixin(function(_json, sk, _fk) {
    return sk;
  }, {
    toString: function() { return '_';}
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
