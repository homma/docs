## インストール
````bash
$ brew install ghc
$ brew install cabal-install
$ cabal update
````

## 配列アクセス
Arr !! index で配列の要素にアクセスできる  
括弧を避けるスタイル

````haskell
"日本語" !! 0
````

## プログラムの基本構造
コンパイルする場合は module を定義する必要がある

````haskell
module Main where

import <Module Name>

main = ...
````
