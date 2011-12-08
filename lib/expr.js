// -*- javascript-indent-level: 4 -*-

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
    match: function(json, sk, fk) {
        return match(this, json, sk, fk);
    },
    or: function(rhs) {
        var lhs = this;
        rhs = lift_value(rhs);
        return mixin(function(json, sk, fk) {
            // stack frame
            return lhs(json, sk, function() {
                return rhs(json, sk, fk);
            });
        });
    },
};

function mixin(f) {
    for (k in base) {
        f[k] = base[k];
    }
    return f;
}

function simple(type) {
    return mixin(function(json, sk, fk) {
        return (typeof json === type) ? sk : fk;
    });
}
module.exports.number = simple('number');
module.exports.string = simple('string');
module.exports.boolean = simple('boolean');

var is_array = Array.isArray ||
    function (value) {
        return {}.toString.call(value).indexOf('Array') >= 0;
    };

function nullable(patterns) {
    // FIXME revisit, of course
    return patterns.length === 0;
}

function sequence(patterns) {
    return mixin(function(json, sk, fk) {
        if (is_array(json)) {
            // FIXME no star yet
            if (patterns.length != json.length) return fk;
            // We can either trampoline on each item of the list, or
            // treat this as a special case, or just recurse on the
            // stack and be done with it. Since we're kind of ignoring
            // some things at the minute, I'll do the last.
            for (var i = 0; i < json.length; i++) {
                if (!match(patterns[i], json[i])) {
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

function object(fields) {
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
        return (json == value) ? sk : fk;
    });
}

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
