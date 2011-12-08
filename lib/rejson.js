var expr = require('./lib/expr');
var parser = require('./lib/parser').parser;
parser.yy = expr;

module.exports = expr.lift;
module.exports.parse = function(str) { return parser.parse(str); };
