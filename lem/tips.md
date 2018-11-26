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

### 環境変数

`LEM_HOME` で Lem のホームディレクトリを設定できます。

### 初期化ファイル

通常は、`~/.lem/init.lisp`。

環境変数 `LEM_HOME` を設定している場合は `${LEM_HOME}/init.lisp`

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

パッケージごとに改行を入れる場合はこうなります。

````lisp
(let ((point (lem:current-point)))
  (dolist (package (list-all-packages))
    (lem:insert-character point #\newline)
    (lem:insert-string point (write-to-string package))))
````

### vi-mode
- `M-x vi-mode` で Vi Mode に入ります。
- 初期設定ファイルに書くことで動作が高速化するようです。

- 実装は完全ではないので、割り引いて考える必要があります。

- `M-x emacs-mode` で元に戻せます。

### OS コマンドの実行

外部ライブラリですが `uiop:run-program` で OS コマンドを実行することができます。

````lisp
;; ls コマンドを実行し、結果をバッファに書く
(lem:insert-string
 (lem:current-point)
 (uiop:run-program "ls" :output :string))
````

ただしどうも実行速度が遅い。  
`ls` コマンドの実行に 2 秒弱かかっている。

コマンドをリストにすると解消します。

````lisp
(uiop:run-program '("ls") :output :string)
````

### バッファの中身を macOS に渡したい　

`C-x #` で `filter-buffer` を実行し、`pbcopy` を指定すればコピーできます。

### バッファメニュー
- 操作を行う前に必ず `g` でリフレッシュします。

### Common Lisp ファイルのロード
- `(load "foo")` で `foo.lisp` ファイルをロードできます。

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
