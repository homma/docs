## Lem の Tips

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