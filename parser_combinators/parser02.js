/*
 *
 * A modified version of a parser combinator described in the
 * following document.
 *
 * http://blog.anatoo.jp/entry/2015/04/26/220026
 */

/*** Parser ***/

const ParseResult = function(success, data, newPosition) {
  this.success = success;
  this.data = data;
  this.position = newPosition;
};

const parse_foo = (input, position) => {
  if (input.substr(position, 3) === "foo") {
    return new ParseResult(true, "foo", position + 3);
  }

  return new ParseResult(false, null, position);
};

const test_foo = (str, pos) => {
  console.log(parse_foo(str, pos));
};

const run_test_foo = () => {
  console.log("# test_foo");
  test_foo("foo", 0);
  test_foo("foo", 1);
  test_foo("bar", 0);
};

// run_test_foo();

const token = str => (input, position) => {
  const len = str.length;
  if (input.substr(position, len) === str) {
    return new ParseResult(true, str, position + len);
  }

  return new ParseResult(false, "Parse Error: token", position);
};

const test_token = (str, pos) => {
  console.log(token("foobar")(str, pos));
};

const run_test_token = () => {
  test_token("foobar", 0);
  test_token("foobar", 1);
};

// run_test_token();

const parse_many_foo = (input, position) => {
  const str = "foo";
  const len = str.length;
  let result = [];

  while (true) {
    if (input.substr(position, len) === str) {
      result.push(str);
      position += len;
    } else {
      break;
    }
  }

  return new ParseResult(true, result, position);
};

const test_many_foo = (str, position) => {
  console.log(parse_many_foo(str, position));
};

const run_test_many_foo = () => {
  console.log("# test_many_foo");
  test_many_foo("foofoobar", 0);
  test_many_foo("foofoobar", 1);
  test_many_foo("oofoobar", 0);
};

// run_test_many_foo();

const many = parser => (input, position) => {
  let result = [];

  while (true) {
    const parsed = parser(input, position);

    if (parsed.success) {
      result.push(parsed.data);
      position = parsed.position;
    } else {
      break;
    }
  }

  return new ParseResult(true, result, position);
};

const test_many = (parser, input, position) => {
  console.log(many(parser)(input, position));
};

const run_test_many = () => {
  console.log("# test_many");
  test_many(token("foo"), "foofoo", 0);
  test_many(token("foo"), "", 0);
  test_many(token("foobar"), "foo", 0);
};

// run_test_many();

const choice = (...parsers) => (input, position) => {
  for (let parser of parsers) {
    const parsed = parser(input, position);
    if (parsed.success) {
      return parsed;
    }
  }

  return new ParseResult(false, "Parse Error: choice", position);
};

const test_choice = (input, position) => {
  const parser = many(choice(token("foo"), token("bar")));

  console.log(parser(input, position));
};

const run_test_choice = () => {
  test_choice("foofoo", 0);
  test_choice("barfoo", 0);
  test_choice("foobaz", 0);
};

// run_test_choice();

const seq = (...parsers) => (input, position) => {
  let result = [];
  let pos = position;

  for (let parser of parsers) {
    const parsed = parser(input, pos);
    if (parsed.success) {
      result.push(parsed.data);
      pos = parsed.position;
    } else {
      return new ParseResult(false, parsed.data, pos);
    }
  }

  return new ParseResult(true, result, pos);
};

const test_seq = (input, position) => {
  const parser = seq(token("foo"), choice(token("bar"), token("baz")));
  console.log(parser(input, position));
};

const run_test_seq = () => {
  test_seq("foobar", 0);
  test_seq("foobaz", 0);
  test_seq("foo", 0);
};

// run_test_seq();

const option = parser => (input, position) => {
  const result = parser(input, position);
  if (result.success) {
    return result;
  }

  return new ParseResult(true, null, position);
};

const test_option = (input, position) => {
  const parser = option(token("foo"));
  console.log(parser(input, position));
};

const run_test_option = () => {
  console.log("# test_option");
  test_option("foo", 0);
  test_option("bar", 0);
};

// run_test_option();

const regex = regexp => (input, position) => {
  let re = regexp;

  if (!re.sticky) {
    re = new RegExp(re.source, regexp.flags + "y");
  }

  re.lastIndex = 0;

  const result = re.exec(input.slice(position));

  if (result) {
    position += result[0].length;
    return new ParseResult(true, result[0], position);
  }

  return new ParseResult(false, "Parse Error: regex", position);
};

const test_regex1 = (input, position) => {
  const parser = regex(/foo/);
  console.log(parser(input, position));
};

const test_regex2 = (input, position) => {
  const parser = regex(/([1-9][0-9]*)/);
  console.log(parser(input, position));
};

const run_test_regex = () => {
  console.log("# test_regex");
  test_regex1("foo", 0);
  test_regex2("2014", 0);
  test_regex2("0001", 0);
};

// run_test_regex();

const lazy = callback => (input, position) => {
  return callback()(input, position);
};

const test_lazy = (input, position) => {
  const parser = option(seq(token("foo"), lazy(() => parser)));

  const result = parser(input, position);

  console.log(result);
  console.log(result.data);
};

const run_test_lazy = () => {
  console.log("# test_lazy");
  test_lazy("foo", 0);
  test_lazy("foofoo", 0);
  test_lazy("foofoofoo", 0);
};

// run_test_lazy();

const map = (parser, fun) => (input, position) => {
  const result = parser(input, position);
  if (result.success) {
    return new ParseResult(result.success, fun(result.data), result.position);
  }

  return result;
};

const test_map = (input, position) => {
  const parser = map(token("hello"), result => result + " parsed.");
  console.log(parser(input, position));
};

const run_test_map = () => {
  console.log("# test_map");
  test_map("hello", 0);
  test_map("foobar", 0);
};

// run_test_map();

const char = chars => (input, position) => {
  const inputChar = input[position];

  // Excludes an empty string since it always matches.
  //
  // ex.)
  // "foo".includes(""); => true
  // "foo".indexOf(""); => 0
  // (new RegExp("")).test("foo"); => true
  //
  if (inputChar == "") {
    return new ParseResult(false, null, position);
  }

  if (chars.includes(inputChar)) {
    return new ParseResult(true, inputChar, position + 1);
  }

  return new ParseResult(false, null, position);
};

const test_char = (input, position) => {
  const parser = char("abcdef");
  console.log(parser(input, position));
};

const run_test_char = () => {
  console.log("# test_char");
  test_char("a", 0);
  test_char("b", 0);
  test_char("bc", 1);
  test_char("g", 0);

  test_char("", 0);
};

// run_test_char();

/*** Arithmatic Calculation Parser ***/

const number = map(regex(/[1-9][0-9]*|[0-9]/), parsed => parseInt(parsed, 10));

const test_number = (input, position) => {
  console.log(number(input, position));
};

const run_test_number = () => {
  console.log("# test_number");
  test_number("0", 0);
  test_number("10", 0);
  test_number("1892", 0);
  test_number("a1", 0);
};

// run_test_number();

const operator = char("+-");

const paren = lazy(() => {
  const LPAREN = "(";
  const RPAREN = ")";

  const parser = seq(token(LPAREN), expression, token(RPAREN));
  return map(parser, parsed => parsed[1]);
});

const atom = choice(number, paren);

const prettify = data => {
  return [data[0]].concat(data[1].reduce((acc, val) => acc.concat(val), []));
};

const expression = map(seq(atom, many(seq(operator, atom))), prettify);

const arith_parser = input => {
  const result = expression(input, 0);
  if (!result.success) {
    console.log(result);
  } else if (input.length !== result.position) {
    console.log(`Parsing failed at ${result.position + 1}.`);
  }

  return result.data;
};

const test_arith_parser = str => {
  console.log("# test_arith_parser");
  console.log(JSON.stringify(arith_parser(str)));
  console.log("");
};

const run_test_arith_parser = () => {
  test_arith_parser("1+2-(3+1-(4))");
  test_arith_parser("1+2-(3+1");
  test_arith_parser("foo");
  test_arith_parser("0-3+(((3)))");
};

// run_test_arith_parser();
