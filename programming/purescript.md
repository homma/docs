# ドキュメント

## PureScript Langage Reference
- https://github.com/purescript/documentation/tree/master/language

## PureScript by Example
- https://leanpub.com/purescript/read
- [実例によるPureScript](https://aratama.github.io/purescript/purescript-book-ja.html)

# 使い方

## インストール

プロジェクト単位でインストールする
````sh
$ mkdir -p ~/Programming/PureScript/my_proj; cd $_
$ npm install purescript pulp bower
$ echo "export PATH=./node_modules/.bin:${PATH}" > ./env.sh
$ chmod +x env.sh
$ . ./env.sh
$ pulp init
$ pulp repl
````

## 配列操作

パッケージのインストールが必要

パッケージのインストール
````sh
$ bower install purescript-arrays
````

動作確認
````purescript
> import Data.Array
> length []
0

> [1] !! 0
(Just 1)

````

[Reference](https://pursuit.purescript.org/packages/purescript-arrays/4.2.2/docs/Data.Array)

## 文字列操作

パッケージのインストールが必要

パッケージのインストール
````bash
$ bower install purescript-strings
````

動作確認
````purescript
> import Data.String
> str = "あいうえお"
> length str
5

> charAt 0 str 
(Just 'あ')

````

日本語の文字列も使用可能  
(!!) は使えない

[Reference](https://pursuit.purescript.org/packages/purescript-strings/3.3.0/docs/Data.String)

# PureScript の問題点

## 基本的な処理でもパッケージの追加インストールが必要
- 配列操作や文字列操作などもパッケージのインストールが必要
- 生成される JavaScript コードをなるべく小さくするためにそうしているのかもしれない

## スクリプト単体で実行できない
- pulp 経由で実行する必要があるみたい
- プロジェクトの作成が必須っぽい

## 依存パッケージが多い
- npm モジュールや bower コンポーネントが大量にインストールされる

## その他
- REPL で FFI を定義できない（外部で定義しいた FFI を REPL から呼び出すことは可能）  
  - そもそも JS で書きたいコード（DOM の操作）は REPL では使用できないので問題とならないかもしれない
- DOM の操作は外部ライブラリが必要
- 基本的に全てをカリー化するので実行速度にペナルティがある
  - 生成された JavaScript では、foo(a, b, c) ではなく foo(a)(b)(c) になるので関数呼び出しが増える
- mkFnX/runFnX で非カリー化を明記する必要がある
- 言語自体がマイナー
