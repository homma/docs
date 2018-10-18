## Lem エディタプログラミング

Lem のバッファ内から Common Lisp を使って Lem を操作する方法をまとめます。

Lem の実装や Emacs の例を参考にしてまとめていますが、内容に間違いがある可能性があります。  
ご注意ください。

## Lem の設定ファイル

Lem の設定ファイルは `~/.lem/init.lisp` です。  
設定は Common Lisp のプログラムとして記述します。  
記述した設定はコンパイルされてエディタにロードされます。

## Lem の関数を探す方法

### lisp-apropos

`M-x lisp-apropos` でキーワードを検索できます。  
`lisp-apropos` からは `q` キーで抜けられます。

### lisp-describe-symbol

`M-x lisp-describe-symbol` でリファレンスを参照できます。  
lisp-mode では `C-c C-d d` で `lisp-describe-symbol` を実行できます。

### Tab キーによる補完

関数名に当たりがついている場合は、関数名の一部を入力してから `Tab` キーを押すこで補完候補から関数を探すことができます。

### Lem の実装を読む

- [ソースコード](https://github.com/cxxxr/lem/tree/master/lib)
- [キーワード検索](https://github.com/cxxxr/lem/search)

`lem-base` や `lem` パッケージの `export` された関数を調べる感じになります。

### Emacs の例を探す

Lem の関数と Emacs の関数は非常に似ているため、参考になります。  
Lem は Common Lisp なので名前空間を意識する必要があります。

### ドキュメントを参照する

- https://github.com/cxxxr/lem/blob/master/document/text.md

