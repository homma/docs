
## リファレンス
- https://docs.microsoft.com/ja-jp/dotnet/fsharp/

### Cheat Sheet
- http://dungpa.github.io/fsharp-cheatsheet/

## Hello, World

````
> printf "hello, world\n";;
````

````
> printfn "hello, world";;
````

## 変数定義

````
> let foo = "foo";;
val foo : string = "foo"
````

## 関数定義

````
> let square x = x * x;;
val qsquare : x:int -> int

> let double x = x + x;;
val double : x:int -> int

> let times x y = x * y;;
val times : x:int -> y:int -> int
````

## 文字列操作

### リファレンス
- https://docs.microsoft.com/ja-jp/dotnet/fsharp/language-reference/strings

### 文字列定義

````
> let str = "あいうえお";;
val str : string = "あいうえお"
````

### 文字列を出力する

````
> printf "foo\n";;
foo
val it : unit = ()
````

````
> printfn "foo";;
foo
val it : unit = ()
````

### 変数に格納した文字列を出力する

文字列を変数に格納した場合、`printf` や `printfn` では直接、文字列を出力できない。

````
> let foo = "foo";;
val foo : string = "foo"

> printfn foo;;

  printfn foo;;
  --------^^^
````

````
> printfn "%s" foo;;
foo
val it : unit = ()
````

````
> System.Console.WriteLine foo;;
foo
val it : unit = ()
````

### 文字数

````
> String.length str;;
val it : int = 5
````

````
> str.Length;;
val it : int = 5
````

### 文字列の連結

````
> let str1 = str + "かきくけこ";;
val str1 : string = "あいうえおかきくけこ"
````

### n 番目の文字の取得

````
> str.[0];;
val it : char = 'あ'
````

````
> str.Chars(0);;
val it : char = 'あ'
````

### 文字列変数の型を確認する

````
> str;;
val it : string = "あいうえお"
````

`GetType()` で型情報を取得できる。

````
> printfn "%s" (str.GetType().Name);;
String
val it : unit = ()
````

- https://en.wikibooks.org/wiki/F_Sharp_Programming/Reflection
- https://stackoverflow.com/questions/9440204/f-printf-string

## 配列

### リファレンス

- https://docs.microsoft.com/ja-jp/dotnet/fsharp/language-reference/arrays

## 参照

````
> let v1 = ref "foo";;
val v1 : string ref = {contents = "foo";}

> v1;;
val it : string ref = {contents = "foo";}

> !v1;;
val it : string = "foo"

> v1 := "bar";;
val it : unit = ()

> v1;;
val it : string ref = {contents = "bar";}

> !v1;;
val it : string = "bar"
````

### リファレンス

- https://docs.microsoft.com/ja-jp/dotnet/fsharp/language-reference/reference-cells

