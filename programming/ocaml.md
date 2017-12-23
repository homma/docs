## インストール
- https://ocaml.org/docs/install.html#macOS

````sh
$ brew install ocaml
$ brew install opam
$ opam init
$ echo 'eval `opam config env`' >> ~/.profile
$ . ~/.profile
````

## ドキュメント

### 学習リソース
- [OCaml チュートリアル](https://ocaml.org/learn/tutorials/index.ja.html)
- [お気楽 OCaml プログラミング入門](http://www.geocities.jp/m_hiroi/func/ocaml.html)
- [Real World OCaml](https://realworldocaml.org/v1/en/html/index.html)
- [User's Manual](https://caml.inria.fr/pub/docs/manual-ocaml/)
- [Developing application with Objective Caml](https://caml.inria.fr/pub/docs/oreilly-book/html/index.html)

## 文法

### コードのエントリーポイント
エントリーポイントが必要なのか、という気もしますが...  
"let () = " でも "let _ = " でも、どちらでも良い

````ocaml
let () = <main program>
let _ = <main program>
````

### 配列アクセス

配列アクセスのリテラルはないみたい

````ocaml
String.get "foo" 0;;
````

### 数値表現

数値にも何種類かある

````ocaml
# 0,0n,0l,0L;;
- : int * nativeint * int32 * int64 = (0, 0n, 0l, 0L)
````

### 数値演算

関数を使用する場合
````ocaml
# Int32.add 1l 2l;;
- : int32 = 3l
# Int32.mul 3l 4l;;
- : int32 = 12l
# Int32.sub 0l 10l;;
- : int32 = -10l
# Int32.div 0l 3l;;
- : int32 = 0l
# Int32.div 0l 0l;;
Exception: Division_by_zero.
````

### 2.0 は 2. と書ける
````ocaml
# 2. ** 8.
  ;;
- : float = 256.
````
- OCaml's exponential operator uses float for its arguments.

### n 進数

2 進数、8 進数、16 進数の表記方法が用意されている
````ocaml
# 0b1, 0o7, 0xF;;
- : int * int * int = (1, 7, 15)
# 0b11 + 0o77 + 0xFF;;
- : int = 321
````

便利

### ターミナル出力

参考 : [Input / Output](http://www2.lib.uchicago.edu/keith/ocaml-class/io.html)

まずは print_endline と read_line から。

入出力チャネルは stdin, stdout, stderr を使用可能。
````ocaml
# stdin, stdout, stderr;;
- : in_channel * out_channel * out_channel = (<abstr>, <abstr>, <abstr>)
````

output_char
````ocaml
# output_char stdout 'a';;
a- : unit = ()
````

output_string
````ocaml
# output_string stdout "こんにちは\n";;
こんにちは
- : unit = ()
````

output
````ocaml
# output stdout "12345" 0 3;;
123- : unit = ()
````

print_string
````ocaml
# print_string "こんにちは\n";;
こんにちは
- : unit = ()
````

print_endline
````ocaml
# print_endline "こんにちは";;
こんにちは
- : unit = ()
# print_endline @@ string_of_int 0;;
0
- : unit = ()
````

print_int
````ocaml
# print_int 0;;
0- : unit = ()
````

print_newline
````ocaml
# print_string "1: ";print_newline ();print_string "2: ";;
1: 
2: - : unit = ()
````

prerr_endline
````ocaml
# prerr_endline "Error: watch out!";;
Error: watch out!
- : unit = ()
````

prerr_bytes, prerr_char, prerr_float, prerr_int, prerr_newline, prerr_string は省略

Printf.printf
````ocaml
# Printf.printf "%s\n" "こんにちは";;
こんにちは
- : unit = ()
````

### ターミナル入力

input_char
````ocaml
# let a = input_char stdin;;
a
val a : char = 'a'
````

input_line
````ocaml
# let ln = input_line stdin;;
hello
val ln : string = "hello"
````

input
````ocaml
# let buf = "12345";;
val buf : string = "12345"
# input stdin buf 1 2;;
ab
- : int = 2
#   buf;;
- : string = "1ab45"
````

read_line
````ocaml
# let ln = read_line ();;
hello
val ln : string = "hello"
````

### リテラル表現
- [Built-In Data Types](http://www2.lib.uchicago.edu/keith/ocaml-class/data.html)
  - 配列は使わないのかな（書きづらそうな記法が割り当てられている）

### 複数行文字列
何も考えなくとも、複数行文字列を作成できる。  
一行にまとめる時は \n でつなぐ。

````ocaml
# {|
  foo
  bar
  baz
  |}
  ;;
- : string = "\nfoo\nbar\nbaz\n"
# "
  foo
  bar
  baz
  " 
  ;;
- : string = "\nfoo\nbar\nbaz\n"
````

### Quoted String
[Quoted String](http://caml.inria.fr/distrib/ocaml-4.04/ocaml-4.04-refman.html#sec250) は、ヒアドキュメント的な機能のようです

````ocaml
# {|foo|};;
- : string = "foo"
````

{tag|string|tag} とすると、マッチする tag のチェックが行われるので、以下のようなことができる。

````ocaml
# {tag|foo|};;|tag};;
- : string = "foo|};;"
````

### 文字コード
- OCaml をカジュアルに使いづらい原因の一つ
  - 今の時代に、Latin-1 決め打ちの文字列は辛い
  - 普通の言語なら、Modernization の名の下に一番最初に修正される箇所

- [Fantasy World OCaml](http://web.archive.org/web/20150103062003/http://chaudhuri.info/misc/fwocaml/)
  - UTF-8 対応は本当にその通り
- [OCaml プログラムで欧文以外の文字を使う方法](https://github.com/camlspotter/ocaml-zippy-tutorial-in-japanese/blob/master/ocaml_i18n.md)
  - ありがたい情報

### 日本語出力
参考 : [OCaml プログラムで欧文以外の文字を使う方法](https://github.com/camlspotter/ocaml-zippy-tutorial-in-japanese/blob/master/ocaml_i18n.md)

````sh
$ vi ~/.ocamlinit
let printer ppf = Format.fprintf ppf "\"%s\"";;
#install_printer printer
````

### 関数合成
- [Pervasives](https://caml.inria.fr/pub/docs/manual-ocaml/libref/Pervasives.html)

- fun1 @@ fun2 @@ fun3 arg で、fun1 (fun2 (fun3 arg)) の意味
- fun1 arg |> fun2 |> fun3 で、fun3 (fun2 (fun1 arg)) の意味

````ocaml
print_endline @@ string_of_int 0;
string_of_int 0 |> print_endline;;
````

- @@ は ML の o や Haskell の . に相当
- 良い感じ

## ツール情報

### コンパイル情報
S 式的なものが出力される
````ocaml
$ ocaml -dlambda
# let foo = "foo";;
(let (foo/1360 = "foo") (apply (field 1 (global Toploop!)) "foo" foo/1360))
val foo : string = "foo"
````

### REPL
ocaml toplevel
- そもそも行編集機能がない

utop
- メタキー打ちづらい
- 余計な表示がある
- メンテナー募集中

rlwrap
- マルチライン編集ができない（-m newline は期待しているのと動きが異なる）
- 補完ができない
- 個人的に readline を使いたくない（GNU が戦略に利用した経緯があるため）

ledit
- 開発終了している模様

### utop
- https://github.com/diml/utop
- https://github.com/realworldocaml/book/wiki/Installation-Instructions
- rlwrap よりも utop を使用することが推奨されているみたい
- core もインストールが推奨されている

````sh
$ opam install core utop
$ utop
````

- #utop_bindings;; するとキーバインドを確認できる
  - 大体は普通の emacs 風のキーバインド

- utop は微妙な感じ。センス！

#### 良いところ
- キーワードや変数を補完してくれる

#### ダメなところ
- macOS だとメタキーが使えない
  - Terminal.app の設定で「メタキーとして Option キーを使用」にチェックを入れると、Option キーがメタキーになる
- Ctrl-P で上の行に移動できるけど、一番上の行にいる時に、一つ前のコードに移動できない
  - Opt をメタキーにして、Opt-P を使用することは可能ですが、Opt-P は押しづらい
- 全角文字には対応していない
  - カーソルの位置は半角文字の文字数で決まっているみたい

- 不要な情報が表示される
  - 時刻が入っている行は不要

### rlwrap
インストール
````sh
$ brew install rlwrap
$ echo 'alias ocaml="rlwrap ocaml"' >> ~/.profile
````

アンインストール
````sh
$ brew uninstall rlwrap readline
$ vi ~/.profile
# alias を消す
````

- rlwrap は行単位での編集しかできない（;; 単位で戻れない）
- シンボルの補完ができない
