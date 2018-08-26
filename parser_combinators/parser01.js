/*
 *
 * A modified version of a parser combinator written in the following document.
 *
 * "You could have invented Parser Combinators"
 * http://theorangeduck.com/page/you-could-have-invented-parser-combinators
 *
 */

/*** StringReader ***/

const ReadResult = function(error, data) {
  this.error = error;
  this.data = data;
};

const StringReader = function(text) {
  this.string = text;
  this.position = 0;
};

StringReader.prototype.read = function() {
  if (this.position == this.string.length) {
    return new ReadResult(true, "Read Error");
  }

  return new ReadResult(false, this.string[this.position]);
};

StringReader.prototype.advance = function(n) {
  this.position += n;
};

StringReader.prototype.getPosition = function() {
  return this.position;
};

StringReader.prototype.setPosition = function(n) {
  this.position = n;
};

/*** Parser ***/

const ParseResult = function(error, data) {
  this.error = error;
  this.data = data;
};

const lit = char => input => {
  const r = input.read();
  if (r.error) {
    return new ParseResult(true, r.data);
  }

  if (r.data == char) {
    input.advance(1);
    return new ParseResult(false, char);
  }

  return new ParseResult(true, "Parse Error");
};

const test1 = char => {
  const parser = lit(char);
  const input = new StringReader("a");

  const result = parser(input);

  console.log(result);
};

const run_test1 = () => {
  console.log("test1");

  test1("a");
  test1("b");
};

run_test1();

const or = (parser0, parser1) => input => {
  const result0 = parser0(input);
  if (!result0.error) {
    return result0;
  }
  const result1 = parser1(input);
  if (!result1.error) {
    return result1;
  }

  return new ParseResult(true, "Parse Error");
};

const test2 = char => {
  const parser = or(lit("a"), lit("b"));
  const input = new StringReader(char);
  const result = parser(input);

  console.log(result);
};

const run_test2 = () => {
  console.log("test2");

  test2("a");
  test2("b");
  test2("c");
};

run_test2();

const and = (parser0, parser1) => input => {
  const pos = input.getPosition();

  const result0 = parser0(input);
  if (result0.error) {
    input.setPosition(pos);
    return new ParseResult(true, result0.data);
  }

  const result1 = parser1(input);
  if (result1.error) {
    input.setPosition(pos);
    return new ParseResult(true, result1.data);
  }

  return [result0, result1];
};

const test3 = str => {
  const parser = and(lit("a"), lit("b"));
  const input = new StringReader(str);
  const result = parser(input);

  console.log(result);
};

const run_test3 = () => {
  console.log("test3");

  test3("ab");
  test3("ac");
  test3("bc");
};

run_test3();

const apply = (fun, parser) => input => {
  const result = parser(input);
  if (result.error) {
    return new ParseResult(true, result.data);
  }
  return new ParseResult(false, fun(result.data));
};

const toInt = char => {
  return char.codePointAt(0);
};

const test4 = str => {
  const parser = or(
    and(apply(toInt, lit("a")), apply(toInt, lit("b"))),
    and(apply(toInt, lit("c")), apply(toInt, lit("d")))
  );
  const input = new StringReader(str);
  const result = parser(input);

  console.log(result);
};

const run_test4 = () => {
  console.log("test4");

  test4("ab");
  test4("bc");
  test4("cd");
};

run_test4();
