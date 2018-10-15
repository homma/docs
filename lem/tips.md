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

### vi-mode
- `M-x vi-mode` で vi-mode に移行できる
- 設定ファイルに書くことで高速化できる

- 実装は完全ではない

- `M-x emacs-mode` で元に戻せる

### バッファメニュー
- 操作を行う前に必ず `g` でリフレッシュする

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
