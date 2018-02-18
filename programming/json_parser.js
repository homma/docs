const StringReader = function(str) {
  this.string = str;
  this.position = 0;
}

StringReader.prototype.read = function() {
  if(this.string.length == this.position) {
    return -1;
  }

  const ret = this.string.codePointAt(this.position);
  this.position++;

  return ret;
}

const parseTest = (parser, src) => {
  const sr = new StringReader(src);
  try {
    console.log(parser(sr));
  } catch(e) {
    console.error(e);
  }
}

const anyChar = tr => {
  const ch = tr.read();
  if(ch >= 0) {
    return String.fromCodePoint(ch)
  } else {
    throw "anyChar: unexpected end of input";
  }
}

// Test
// parseTest(anyChar, "abc");
// parseTest(anyChar, "");

const plist = list => tr => list.map(v => v(tr));

// Test
// parseTest(plist([anyChar, anyChar]), "abc");
// parseTest(plist([anyChar, anyChar]), "a");

StringReader.prototype.peek = function() {
  if(this.string.length == this.position) {
    return -1;
  }

  const ret = this.string.codePointAt(this.position);
  return ret;
}

const peek = tr => {
  const ch = tr.peek();
  if(ch >= 0) {
    return String.fromCodePoint(ch)
  } else {
    throw "peek: unexpected end of input";
  }
}

// Test
// parseTest(plist([anyChar, peek, anyChar]), "abc");

const isOneOf = str => tr => {
  const ch = tr.peek();
  if(ch == -1 || str.indexOf(String.fromCodePoint(ch)) < 0 ) {
    return false;
  } else {
    tr.read();
    return true;
  }
}

// Test
// parseTest(isOneOf("ab"), "abc");
// parseTest(isOneOf("ab"), "def");

const oneOf = str => tr => {
  const ch = tr.peek();
  if(isOneOf(str)(tr)) {
    return String.fromCharCode(ch);
  } else {
    throw `oneOf: ${String.fromCharCode(ch)} is not in ${str}`
  }
}

// Test
// parseTest(oneOf("ab"), "abc");
// parseTest(oneOf("ab"), "def");

const StringWriter = function() {
  this.string = new String();
}

StringWriter.prototype.write = function(ch) {
  this.string = this.string + ch;
}

StringWriter.prototype.toString = function() {
  return this.string;
}

const isDigit = ch => {
  const res = "0123456789".indexOf(ch);
  if(res >= 0) {
    return true;
  } else {
    return false;
  }
}

const many = f => tr => {
  const sw = new StringWriter();
  const g = () => {
    const ch = tr.peek();
    if( (ch >= 0) && f(String.fromCharCode(ch)) ) {
      sw.write(String.fromCharCode(ch));
      tr.read();
      g();
    }
  }
  g();
  return sw.toString();
}

// Test
// parseTest(many(isDigit), "123abc");

const isSpace = ch => " \r\n\t".indexOf(ch) >= 0

const spaces = tr => {
  const ch = tr.peek();
  if( ch >= 0 && isSpace(String.fromCharCode(ch)) ) {
    tr.read();
    spaces(tr);
  }
}

const skipAndTake = (a, b) => tr => { a(tr); return b(tr) }
const takeAndSkip = (a, b) => tr => { const ret = a(tr); b(tr); return ret }

// Test
parseTest( skipAndTake(spaces, anyChar), "   123");
parseTest( plist([takeAndSkip(anyChar, spaces), anyChar]), "1   23");
