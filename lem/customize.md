## Common Lisp を使用した Lem のカスタマイズ
- 主に Lem 固有のプログラムについてまとめます

## 設定ファイル
- `~/.lem/init.lisp`

## 関数を探す
- `M-x lisp-apropos` でキーワード検索
  - `lisp-apropos` からは `q` キーで抜けられます

- `M-x lisp-describe-symbol` でリファレンス参照
  - lisp-mode では `C-c C-d d` で `lisp-describe-symbol` を実行できます

- 実装を読む
- Emacs の例を探す

## ポイント

ポイント（カーソル位置の情報）を使用したプログラム。

### よく使用する関数

#### 現在のポイントを確認する

`current-point` を使用します。

````lisp
;; current-point
(let ((point (lem-base:current-point)))
  (something-to-do))
````

#### 現在のポインタ情報を保存してから操作を行う

`save-excursion` を使用します。

````lisp
;; save-excursion
(lem-base:save-excursion
 (something-to-do))
````

### 関数の作例

#### 現在のポイントに文字列を挿入する

ポイントの位置を保持する
````lisp
;; insert-here
(defun insert-here (str)
  (lem-base:save-excursion
   (lem-base:insert-string (lem-base:current-point) str)))
(insert-here "foobarbaz")
````

ポイントの位置を挿入した文字の後ろに移動する
````lisp
;; insert-here-and-move
(defun insert-here-and-move (str)
  (lem-base:insert-string (lem-base:current-point) str)) 
(insert-here-and-move "foobarbaz")
````

#### 現在のポイントの次の行に文字列を挿入する

`line-end` で行末に移動します。

ポイントの位置を保持する
````lisp
;; insert-next-line
(defun insert-nextline (str)
  (lem-base:save-excursion
   (let ((point (lem:line-end (lem-base:current-point))))
     (lem-base:insert-character point #\newline)
     (lem-base:insert-string point str))))

(insert-nextline "foobarbaz")
````

ポイントの位置を挿入した文字の後ろに移動する
````lisp
;; insert-next-line
(defun insert-nextline-and-move (str)
  (let ((point (lem:line-end (lem-base:current-point))))
    (lem-base:insert-character point #\newline)
    (lem-base:insert-string point str)))

(insert-nextline-and-move "foobarbaz")
````

## モードライン

### バッファ名の取得

````lisp
(lem:modeline-name lem::*current-window*)
````

