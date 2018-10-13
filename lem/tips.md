## Lem の Tips

### インストール

- macOS の場合は brew で Roswell をインストールして ros で lem をインストールする
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
