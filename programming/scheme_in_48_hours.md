
# 2017.12.19

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
