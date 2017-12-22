## インストール

インストールされるサイズは 1GB!

````bash
$ brew install ghc
$ brew install cabal-install
$ cabal update
````

Cabal はパッケージのインストールに使用します

## 配列操作

[a, b, c] で配列を作成

a : [arr] で cons

[a] ++ [b] で append

length arr で配列のサイズを確認

arr !! index で配列の要素にアクセス（括弧を避けるスタイル）

````haskell
"日本語" !! 0
````

## セクション

中置演算子の部分適用ができる

````haskell
($ 0) show
(show $) 0
(++ "post") "pre"
("pre" ++) "post"
(/ 4) 12
(12 /) 4
((-) 2) 4
(4 -) 2
````

## プログラムの基本構造

コンパイルする場合は module を定義する必要がある

````haskell
module Main where

import <Module Name>

main = ...
````

## ghci の使い方

:h でヘルプ

:t で型を調べられる
````
Prelude> :t show
show :: Show a => a -> String
````

# Haskell の問題点

## エラーメッセージが不親切
- 問題発生箇所はわかりますが、どう直せば良いのかエラーメッセージから判別できないことが多い

## 静的型付け
- どうしても型のためにコーディングしている感じになる（型を変換するためだけのコードなど）
- とりあえず動くところまで持っていくのに時間がかかる

## 開発環境のバイナリが大きい
- インストールされるバイナリが 1GB もあるので、軽量環境では運用しづらい（例えば Docker コンテナ、スマートフォン、ウェブブラウザなど）

## 記号が多い
- 記号を使用した特殊な演算子を多用するため、ウェブの検索エンジンで検索がしづらい

## Camel Case
- https://wiki.haskell.org/Camel_case
- これは好みの問題ですが、大文字が多くなるので、ソースコードが読みにくい
- [Miranda](https://en.wikipedia.org/wiki/Miranda_(programming_language)) は Snake Case っぽい
