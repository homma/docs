
## ドキュメント

### リファレンス
- [F# のガイド](https://docs.microsoft.com/ja-jp/dotnet/fsharp/)
- [F# 言語リファレンス](https://docs.microsoft.com/ja-jp/dotnet/fsharp/language-reference/index)
- [F# Core Library Reference](https://msdn.microsoft.com/visualfsharpdocs/conceptual/fsharp-core-library-reference)

### Cheat Sheet
- http://dungpa.github.io/fsharp-cheatsheet/

### Articles
- [F# Programming](https://en.wikibooks.org/wiki/F_Sharp_Programming)
- [Real-World F#](http://tomasp.net/blog/real-world-book-msdn.aspx/)
- [F# for Fun and Profit](https://swlaschin.gitbooks.io/fsharpforfunandprofit/content/)

### Language Design Process
- https://github.com/fsharp/fslang-design

## Hello, World

````fsharp
> printf "hello, world\n";;
hello, world
val it : unit = ()
````

````fsharp
> printfn "hello, world";;
hello, world
val it : unit = ()
````

````fsharp
> System.Console.WriteLine "hello, world";;
hello, world
val it : unit = ()
````

## コメント

````fsharp
// one line comment
(*
 * multi-line comment
 *)
````

## 変数

### 変数を定義する

````fsharp
> let foo = "foo";;
val foo : string = "foo"
````

### 型を指定する

````fsharp
> let str : string = "foo";;
val str : string = "foo"

> let n : int = 2;;
val n : int = 2
````

### 変数の型を確認する

````fsharp
> let str = "あいうえお";;
val str : string = "あいうえお"

> str;;
val it : string = "あいうえお"
````

`GetType()` で型情報を取得できる。

````fsharp
> printfn "%s" (str.GetType().Name);;
String
val it : unit = ()
````

- https://en.wikibooks.org/wiki/F_Sharp_Programming/Reflection
- https://stackoverflow.com/questions/9440204/f-printf-string

## 関数

### 関数を定義する

```fsharp
> let square x = x * x;;
val square : x:int -> int

> let double x = x + x;;
val double : x:int -> int

> let times x y = x * y;;
val times : x:int -> y:int -> int
````

### クロージャを使用して関数を定義する

````fsharp
> let square = fun x -> x * x;;
val square : x:int -> int

> square 2;;
val it : int = 4
````

## クロージャ

### クロージャを使用する

````fsharp
> (fun x -> x * x) 2;;
val it : int = 4
````

## 文字列操作

### リファレンス
- https://docs.microsoft.com/ja-jp/dotnet/fsharp/language-reference/strings

### 文字列定義

```fsharp
> let str = "あいうえお";;
val str : string = "あいうえお"
````

### 文字列を出力する

````fsharp
> printf "foo\n";;
foo
val it : unit = ()
````

````fsharp
> printfn "foo";;
foo
val it : unit = ()
````

### 変数に格納した文字列を出力する

文字列を変数に格納した場合、`printf` や `printfn` では直接、文字列を出力できない。

````fsharp
> let foo = "foo";;
val foo : string = "foo"

> printfn foo;;

  printfn foo;;
  --------^^^
````

代わりに、書式指定して文字列を出力する。

````fsharp
> printfn "%s" foo;;
foo
val it : unit = ()
````

`WriteLine` を使用することも可能。

````fsharp
> System.Console.WriteLine foo;;
foo
val it : unit = ()
````

### 文字数をカウントする

````fsharp
> String.length str;;
val it : int = 5
````

````fsharp
> str.Length;;
val it : int = 5
````

### 文字列を連結する

````fsharp
> let str1 = str + "かきくけこ";;
val str1 : string = "あいうえおかきくけこ"
````

### n 番目の文字を取得する

````fsharp
> str.[0];;
val it : char = 'あ'
````

````fsharp
> str.Chars(0);;
val it : char = 'あ'
````

### String Interpolation

未実装の模様。

- https://github.com/dotnet/fsharp/pull/6770
- https://github.com/fsharp/fslang-design/issues/6
- https://github.com/fsharp/fslang-design/blob/master/RFCs/FS-1001-StringInterpolation.md

## 配列

### リファレンス

- https://docs.microsoft.com/ja-jp/dotnet/fsharp/language-reference/arrays

## 参照

````fsharp
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

## 代入

````fsharp
> let mutable v2 = "foo";;
val mutable v2 : string = "foo"

> v2 <- "bar";;
val it : unit = ()

> v2;;
val it : string = "bar"

> printfn "%s" v2;;
bar
val it : unit = ()

````

### リファレンス

- https://docs.microsoft.com/ja-jp/dotnet/fsharp/language-reference/values/

## 演算子

### 関数パイプライン

````fsharp
> printfn "%d" <| String.length "日本語";;
3
val it : unit = ()

> "日本語".Length |> printfn "%d";;
3
val it : unit = ()
````

### リファレンス

- https://docs.microsoft.com/ja-jp/dotnet/fsharp/language-reference/symbol-and-operator-reference/

