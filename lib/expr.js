function FAIL() { return false; };
function SUCCEED() { return true; };

function match(pattern, json) {
    var k = pattern(json, SUCCEED, FAIL);
    // ka-boing!
    while (typeof k === 'function') {
        k = k();
    }
    return k;
}
module.exports.match = match;

function simple(type) {
    return function() {
        return function(json, sk, fk) {
            return (typeof json === type) ? sk : fk;
        }
    }
}
module.exports.number = simple('number');
module.exports.string = simple('string');
module.exports.boolean = simple('boolean');

function or(lhs, rhs) {
    return function(json, sk, fk) {
        // stack frame
        return lhs(json, sk, function() {
            return rhs(json, sk, fk);
        });
    }
}
module.exports.or = or;

var is_array = Array.isArray ||
    function (value) {
        return {}.toString.call(value).indexOf('Array') >= 0;
    };
module.exports.is_array = is_array;

function nullable(patterns) {
    // FIXME revisit, of course
    return patterns.length === 0;
}

function sequence(patterns) {
    return function(json, sk, fk) {
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
    }
}
module.exports.seq = sequence;
