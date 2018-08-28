/*
 * A port of the following parser implemented in Java into JavaScript
 * 
 * http://kmizu.hatenablog.com/entry/20090225/1235576560
 * 
 */

const ParseResult = function(value, rest) {
  this.value = value;
  this.rest = rest;
};

// unused
ParseResult.prototype.print = function() {
  return JSON.stringify(`(value = ${this.value}, rest = ${this.rest}`);
};

const string = param => input => {
  if (input.startsWith(param)) {
    return new ParseResult(param, input.substr(param.length));
  }

  return null;
};

// this itself is a parser. not returning a parser.
const any = input => {
  if (input.length > 0) {
    return new ParseResult(input.substr(0, 1), input.substr(1));
  }

  return null;
};

const or = (parser1, parser2) => input => {
  const result1 = parser1(input);
  if (result1 != null) {
    return result1;
  }

  return parser2(input);
};

const seq = (parser1, parser2) => input => {
  const result1 = parser1(input);
  if (result1 == null) {
    return null;
  }
  const result2 = parser2(result1.rest);
  if (result2 == null) {
    return null;
  }
  return new ParseResult([result1.value, result2.value], result2.rest);
};

const seqN = (...parsers) => input => {
  let result = [];
  for (let parser of parsers) {
    const res = parser(input);
    if (res == null) {
      return null;
    }
    result.push(res.value);
    input = res.rest;
  }
  return new ParseResult(result, input);
};

const notp = parser => input => {
  const result = parser(input);
  if (result == null) {
    return new ParseResult(null, input);
  }

  return null;
};

const andp = parser => notp(notp(parser));

const opt = parser => or(parser, apply(string(""), param => null));

const rep1 = parser =>
  apply(seq(parser, rep(parser)), param => {
    param[1].push(param[0]);
    return param[1];
  });

const rep = parser => input => {
  let result = [];
  while (true) {
    const res = parser(input);
    if (res == null) {
      return new ParseResult(result, input);
    }
    result.push(res.value);
    input = res.rest;
  }
};

const apply = (parser, fun) => input => {
  const result = parser(input);
  if (result == null) {
    return null;
  }
  return new ParseResult(fun(result.value), result.rest);
};

/*** test ***/

const rec = callback => input => {
  return callback()(input);
};

const test = str => {
  console.log("# test: a^n b^n c^n n >= 1");

  const A = rec(() => seqN(string("a"), opt(A), string("b")));
  const B = rec(() => seqN(string("b"), opt(B), string("c")));
  const S = seqN(
    andp(seq(A, notp(string("b")))),
    rep1(string("a")),
    B,
    notp(string("c"))
  );

  console.log(JSON.stringify(S(str)));
  console.log();
};

const run_test = () => {
  test("abc");
  test("aaabbbccc");
  test("aaabbbcc");
};

// run_test();
