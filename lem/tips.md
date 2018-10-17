## Lem の Tips

### Lem について
- Emacs 系のエディタ
- 起動が高速なため、常駐させる必要がない
- Common Lisp で実装されており、拡張も Common Lisp で書ける
- Common Lisp はプログラムをネイティブコンパイルするので、プラグインの実行速度も高速
- vi 互換モードも実装されている

- Lem の意味は不明
  - Lisp Editor Macros?

### ウェブサイト
- 開発ページ
  - https://github.com/cxxxr/lem

### インストール

- macOS の場合は brew で Roswell をインストールし、ros コマンドで lem をインストールする
````sh
$ brew install roswell
$ ros install cxxxr/lem
$ export PATH=${PATH}:${HOME}/.roswell/bin
$ lem
````

### プログラムの出力をバッファ内に書き込む

`current-point` でカーソル位置を取得し、`insert-string` で書き込みます。

````lisp
(lem:insert-string (lem:current-point) "foo")
````

改行を入れてから出力する場合は `insert-character` で改行を先に出力しておきます。

````lisp
(let ((point (lem:current-point)))
  (lem:insert-character point #\newline)
  (lem:insert-string point "foo"))
````

### パッケージの一覧をバッファ内に出力する

パッケージの一覧を `write-to-string` で文字列に変換し、`insert-string` でバッファ内に書き込みます。

````lisp
(let ((point (lem:current-point)))
  (lem:insert-character point #\newline)
  (lem:insert-string point
                     (write-to-string (list-all-packages))))
````

### vi-mode
- `M-x vi-mode` で Vi Mode に入ります。
- 初期設定ファイルに書くことで動作が高速化するようです。

- 実装は完全ではないので、割り引いて考える必要があります。

- `M-x emacs-mode` で元に戻せます。

### バッファメニュー
- 操作を行う前に必ず `g` でリフレッシュします。

### ライブラリのインストール
- Roswell でインストールする

````sh
$ ros install ...
````

- 試していないので要確認

### ライブラリのロード
- `M-x load-library` でライブラリをロードする

### ライブラリの公開
- 要確認

### キーボードイベントの登録

キーボード入力に対して関数を登録したい場合は `define-key` を使用する。

`define-key` はキーマップを設定するためのものという印象が強いですが、GUI アプリでキーボードイベントの設定をしているのと同じ。
