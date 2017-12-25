JavaScript より、もう少し簡潔に書きたい。

## AltJS で気になるポイント
- 開発が活発
- 今後も開発が継続されることが期待できる
- REPL がある
- 日本語が使える（OCaml 系は Unicode に対応していないことが多い）
- ブラウザ上でコンパイルできる
- JavaScript よりも記述が簡潔
- コンパイルされた JavaScript ファイルが小さい
- ランタイムライブラリ不要（もしくは小さい）
- 十分な実行速度

この条件に一番近い言語が JavaScript だという...  
JavaScript 自体の更新が頻繁なので、同じ速度以上で開発が続いていないと難しい  
2015 年より前に開発が止まってしまっていると、fat arrow, generator, async/await などに対応していない

## 感想
Haste が良さそうですが、開発はそれほど活発ではない感じ（既に十分完成しているということかもしれませんが）。  
個人で開発しているみたい。  
性能はいいみたいだし、FFI もインラインで書けて簡単そう。  
Haskell なので、言語の情報はたくさんある。  
call by need が感覚的によくわからない。

PureScript は開発も活発で面白そう。  
Haskell 系の文法の言語は経験がないので試してみたい。  
AltJS なのに、DOM をいじるライブラリがデフォルトでビルトインされていないみたいなのが不思議。  
基本的に全てカリー化されるため、実行速度は遅い。  
何もしないと、foo(a,b,c) ではなく foo(a)(b)(c) になる。  
mkFnX や runFnX を使えば解消可能ではある。  
ループの内側で関数を呼ぶ時や、ループの内側から呼ばれる関数でのみ mkFnX/runFnX を使うと良さそう。
FFI はインラインで書けないので Haste より少し面倒（分離した方が綺麗というのはその通りなのですが）。  
REPL では FFI を定義できない（外部で定義しいた FFI を REPL から呼び出すことは可能）。  
生成されるコードは可読性があり、サイズもコンパクト。

現実的に有用そうなのは Scala.js と Kotlin かな。  
開発が止まる可能性は低いので安心感があるし、比較的簡潔に書けそう。  
Scala はマクロがある。  
より関数型言語を志向している。  
Scala Native でネイティブバイナリにコンパイルできる（wasm にはコンパイルできない）。  
Kotlin は wasm やネイティブバイナリにコンパイルできる。

Elm は List.map と書かないといけないのが辛い。  
FFI も大変そう。

## 実装
- [CoffeeScript](https://github.com/jashkenas/coffeescript)
- [Scala.js](https://github.com/scala-js/scala-js)
- [Kotlin](https://github.com/JetBrains/kotlin/tree/master/js)
- [PureScript](https://github.com/purescript/purescript)
- [Haste](https://github.com/valderman/haste-compiler)
- [Elm](https://github.com/elm-lang/elm-compiler)
- [ClojureScript](https://github.com/clojure/clojurescript)
- [LiveScript](https://github.com/gkz/LiveScript)
- [Haxe](https://github.com/HaxeFoundation/haxe)
- [TypeScript](https://github.com/Microsoft/TypeScript)

- [UHC](https://github.com/UU-ComputerScience/uhc/tree/master) - [The Utrecht Haskell Compiler JavaScript Backend](https://uu-computerscience.github.io/uhc-js/#what_is_uhcjs)
- [GHCJS](https://github.com/ghcjs/ghcjs)
- [Idris](https://github.com/idris-lang/Idris-dev) JavaScript 上の仮想マシンで動くみたい
- [Fay](https://github.com/faylang/fay)
- [SMLtoJs](https://github.com/melsman/mlkit/blob/master/README_SMLTOJS.md)
- [BuckleScript](https://github.com/BuckleScript/bucklescript) 日本語が使えないので。。。
- [GopherJS](https://github.com/gopherjs/gopherjs)
- [Shift.JS](https://github.com/shift-js/shift-js)

## AltJS の一覧
- https://github.com/jashkenas/coffeescript/wiki/List-of-languages-that-compile-to-JS

## AltJS を探している時に見つけたもの
- [Little Smallscript](https://github.com/ympbyc/LittleSmallscript)
- [SqueakJS](https://github.com/bertfreudenberg/SqueakJS) [squeak.js.org](https://squeak.js.org)
- [Amber](https://lolg.it/amber/amber) [www.amber-lang.net](http://www.amber-lang.net)
