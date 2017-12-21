## インストール

インストールされるサイズは 1GB!

````bash
$ brew install ghc
$ brew install cabal-install
$ cabal update
````

Cabal はパッケージのインストールに使用します

## 配列アクセス

Arr !! index で配列の要素にアクセスできる  
括弧を避けるスタイル

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

:t で型を調べられる
````
Prelude> :t show
show :: Show a => a -> String
````
