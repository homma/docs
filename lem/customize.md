## Common Lisp を使用した Lem のカスタマイズ
- 主に Lem 固有のプログラムについてまとめます

## 設定ファイル
- `~/.lem/init.lisp`

## 関数を探す
- `M-x lisp-apropos` でキーワード検索
- `lisp-apropos` からは `q` キーで抜けられます

- `M-x lisp-describe-symbol` でリファレンス参照

- 実装を読む

## ポイント

### 現在のポイントを確認する

````lisp
(lem-base:current-point)
````

### 現在のポイントに文字列を挿入する

````lisp
;; insert-here
(defun insert-here (str) (lem-base:insert-string (lem-base:current-point) str))
(insert-here "foobarbaz")
````

## モードライン

`(lem:modeline-name lem::*current-window*)` でバッファ名を取得できる
