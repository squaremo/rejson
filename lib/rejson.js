var expr = require('./expr');
var parser = require('./parser').parser;
parser.yy = expr;

module.exports = expr.lift;
module.exports.parse = function(str) { return parser.parse(str); };
