
## General

### Lexing.lexeme lexbuf

`Lexing.lexeme lexbuf` は正規表現にマッチした文字列。

- https://caml.inria.fr/pub/docs/manual-ocaml/libref/Lexing.html
````
val lexeme : lexbuf -> string
Lexing.lexeme lexbuf returns the string matched by the regular expression.
````

### token lexbuf

`token lexbuf` は該当する文字列を無視して次の文字列をスキャンする指示。
