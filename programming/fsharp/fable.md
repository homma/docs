
## About

- Better JavaScript として Fable を使用したい

## インストール

````sh
$ npm install fable-compiler
````

## 特徴

- 出力される JavaScript の可読性も配慮されているとのこと

## オンライン REPL

https://fable.io/repl/

Fable を簡単に試すことができる。  
補完やサジェスションが便利。  
コンパイルされた JavaScript を参照できる。  
コンソールのログも見られる。  
生成されたウェブページはなぜか表示されなかった。

## 使い方

### ドキュメント出力

#### document.write

`document.write` は以下のように書ける。

````fsharp
open Browser

document.write "<div>foo</div>"
````

JavaScript にコンパイルするとこうなる。

````javascript
document.write("<div>foo</div>");
````

完全修飾でも記述可能。

````fsharp
Browser.Dom.document.write "<div>foo</div>"
````

### コンソール出力

コンソール出力関数がそのまま使用可能。  
使用する関数によって、出力される JavaScript が異なる。

#### printfn

````fsharp
printfn "hello, world"
````

この場合、`console.log` は使用されないみたい。  
書式付き出力であるためだと思われる。

````javascript
import { toConsole, printf } from "fable-library/String.js";
toConsole(printf("hello, world"));
````

#### System.Console.WriteLine

````fsharp
System.Console.WriteLine "hello, world"
````

こちらは `console.log` になる。

````javascript
console.log("hello, world");
````

#### Browser.Dom.console.log

ウェブブラウザのみをターゲットとしている場合は、このように記載することも可能。

````fsharp
open Browser

console.log "hello, world"
````

JavaScript にコンパイルするとこうなる。

````javascript
console.log("hello, world");
````

こう書くことも可能。

````fsharp
Browser.Dom.console.log "hello, world"
````

数字を出力可能。

````fsharp
Browser.Dom.console.log 42
````

### 日本語

これはちゃんと 3 になる。

````fsharp
printfn "%d" <| "日本語".Length
// => 3
````

JavaScript の `String#length` が呼ばれているので、分かってしまえば当然ではある。

````javascript
import { toConsole, printf } from "fable-library/String.js";
toConsole(printf("%d"))("日本語".length);
````

文字列に添字でアクセスした場合も問題なし。

````fsharp
"日本語".[0] |> System.Console.WriteLine
// => 日
````

JavaScript はこうなる。

````javascript
console.log("日本語"[0]);
````

### コメント

コメントは削除される。

````fsharp
// this is a comment.
(* this is also a comment. *)
````

### 変数

変数に値を入れると `const` が使用される。

F#
````fsharp
let foo = "foo"
````

JavaScript
````javascript
export const foo = "foo";
````

### クロージャ

アロー演算子を使わないのは、Babel の設定のせいかな?

F#
````fsharp
fun x -> x * x
````

JavaScript
````javascript
(function (x) {
  return x * x;
});
````

F#
````fsharp
let square = fun x -> x * x
````

JavaScript
````javascript
export function square(x) {
  return x * x;
}
````

### 文字列補完

- F# にはまだ String Interpolation はないので、`` ` `` を使った文字列補完はできなさそう

### async / await

- 要調査

