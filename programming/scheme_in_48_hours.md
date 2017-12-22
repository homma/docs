[48時間でSchemeを書こう](https://ja.wikibooks.org/wiki/48時間でSchemeを書こう) を読みます

# 2017.12.21
($ args) はセクション（中置演算子の部分適用）でした

````haskell
Prelude> :t ($ 0)
($ 0) :: Num a => (a -> b) -> b
Prelude> :t (+ 0)
(+ 0) :: Num a => a -> a
Prelude> :t (++ "a")
(++ "a") :: [Char] -> [Char]
````

# 2017.12.20

## Setup
コンパイルをすると parsec がないと言われるので、パッケージマネージャと Parsec パッケージをインストールします
````
$ ghc -package parsec -o simple_parser simple_parser.hs
<command line>: cannot satisfy -package parsec
    (use -v for more information)
````

Haskell のバイナリは 1GB もあるのに、パッケージマネージャもパッケージも入っていなかったみたい。

Cabal をインストール
````
$ brew install cabal-install
$ cabal update
````

Parsec をインストール
````
$ cabal install parsec
````

## simple_parser.hs
コードが長くなってきました

````haskell
-- compile: $ ghc -package parsec simple_parser.hs

module Main where

import System.Environment
import Text.ParserCombinators.Parsec hiding (spaces)

-- symbol parser
symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

-- parser
readExpr :: String -> String
readExpr input = case parse symbol "lisp" input of
  Left err -> "No match: " ++ show err
  Right val -> "Found value"

main :: IO ()
main = do
  args <- getArgs
  putStrLn (readExpr (args !! 0))
````

## simple_parser2.hs 
````haskell
-- compile: $ ghc -package parsec simple_parser2.hs

module Main where

import System.Environment
import Text.ParserCombinators.Parsec hiding (spaces)

-- symbol parser
symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

-- spaces parser
spaces :: Parser ()
spaces = skipMany1 space

-- parser
readExpr :: String -> String
readExpr input = case parse (spaces >> symbol) "lisp" input of
  Left err -> "No match: " ++ show err
  Right val -> "Found value"

main :: IO ()
main = do
  args <- getArgs
  putStrLn (readExpr (args !! 0))
````

# 2017.12.19

## Setup
Haskell をインストール

````
$ brew install ghc
````

## hello.hs
````haskell
module Main where
import System.Environment

main :: IO ()
main = do
  args <- getArgs
  putStrLn ("Hello, " ++ args !! 0)
````

## exercise 1

### 1-1
````haskell
module Main where
import System.Environment

main = do
  args <- getArgs
  putStrLn (args !! 0 ++ " & " ++ args !! 1)
````

インデントは、空白 4 個がスタンダードなのかな?

### 1-2
````haskell
module Main where
import System.Environment

main = do
  args <- getArgs
  let first = read (args !! 0)
  let second = read (args !! 1)
  putStrLn $ show ( first + second )
````

first <- read (args !! 0) だとエラーになる  
どうしてエラーになったのかはエラーメッセージからは読み解けない  
型が違うのは理解できましたが、そこらへんは上手くやってもらいたい。。。

### 1-3
````haskell
module Main where
import System.Environment
import System.IO

main = do
  putStr "Enter Your Name: "
  hFlush stdout
  name <- getLine
  putStrLn $ "Your Name is " ++ name
````

出力バッファをフラッシュするには、System.IO を import して、hFlush stdout する  
let name = getLine とするとコンパイルエラーになる  
Haskell の型を理解していないので、エラーメッセージの意味がよくわからない
