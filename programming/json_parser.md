# About
- パーサーコンビネータの実装の練習として、参考ドキュメントを参考にさせていただき、JSON パーサーを実装してみます

# 参考ドキュメント
- https://qiita.com/7shi/items/04c2991239894687ef2f

# 再実装 #1
- 写経モードで参考ドキュメントの実装を JavaScript に置き換えます

## 実装方針
- Node.js で実行できるようにコーディングする
- 基本的に実直な再実装とする
  - 元のコードの流れをなるべく変更しない
- 一度全ての再実装が終わったら、2 周目はより自然な実装方法を考える

## 簡易パーサー

### 1 文字読み取り
- StringReader みたいな状態の塊を使うのは関数型っぽくない感じ
  - パーサーの状態は引数と返り値でやり取りする方が関数型っぽい
- 1 文字読み出しが Read なのは違和感がある
  - readOne にしたいけど、ここは read にしておきます
- StringReader.Read は 32bit Int を返す
  - Unicode にどう対応しているのか不明
- StringReader.Read は、文字列の最後まで到達している場合は -1 を返す
  - これに近いのは codePointAt かな
- JavaScript だと、try/catch はあまり使わない印象
  - 今まで try/catch を使った記憶がないです

````javascript
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

parseTest(anyChar, "abc");
parseTest(anyChar, "");
````

実行結果
````javascript
a
anyChar: unexpected end of input
````

### 連続読み取り

- JavaScript では自動的にカリー化されないので、 plist はカリー化済みの関数として定義します

````javascript
const plist = list => tr => list.map(v => v(tr));

parseTest(plist([anyChar, anyChar]), "abc");
parseTest(plist([anyChar, anyChar]), "a");
````

実行結果
````javascript
[ 'a', 'b' ]
anyChar: unexpected end of input
````

### 文字確認

````javascript
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

parseTest(plist([anyChar, peek, anyChar]), "abc");
````

実行結果
````javascript
[ 'a', 'b', 'b' ]
````

### 指定した文字か確認

- isOneOf もカリー化済みの関数として定義します

````javascript
const isOneOf = str => tr => {
  const ch = tr.peek();
  if(ch == -1 || str.indexOf(String.fromCodePoint(ch)) < 0 ) {
    return false;
  } else {
    tr.read();
    return true;
  }
}

parseTest(isOneOf("ab"), "abc");
parseTest(isOneOf("ab"), "def");
````

実行結果
````javascript
true
false
````

### 指定した文字の読み取り

````javascript
const oneOf = str => tr => {
  const ch = tr.peek();
  if(isOneOf(str)(tr)) {
    return String.fromCharCode(ch);
  } else {
    throw `oneOf: ${String.fromCharCode(ch)} is not in ${str}`
  }
}

parseTest(oneOf("ab"), "abc");
parseTest(oneOf("ab"), "def");
````

実行結果
````javascript
a
oneOf: d is not in ab
````

### 条件に合う限り読み続ける

- StringWriter を実装
- isDigit を実装

````javascript
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
parseTest(many(isDigit), "123abc");
````

実行結果
````javascript
123
````

### 空白の読み飛ばし
````javascript
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
````

実行結果
````javascript
1
[ '1', '2' ]
````

## JSON パーサー

### 文字列の読み取り

### 数値の読み取り

### 値の読み取り

### 本体

# 再実装 #2

## 実装方針
- JavaScript らしく記述する

- StringReader/StringWriter を使うよりも、引数と返り値で情報を渡したい
- JavaScript の文字列に合わせた実装にする
  - codePointAt, fromCodePoint は使用しない
- try/catch は使用しない
- 再帰もしない

## 簡易パーサー
### 1 文字読み取り
### 連続読み取り
### 文字確認
### 指定した文字か確認
### 指定した文字の読み取り
### 条件に合う限り読み続ける
### 空白の読み飛ばし

## JSON パーサー
### 文字列の読み取り
### 数値の読み取り
### 値の読み取り
### 本体

