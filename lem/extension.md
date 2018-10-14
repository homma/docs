## Common Lisp を使用した Lem の拡張

主に Lem 固有のプログラムについてまとめます。

Lem の実装や Emacs の例を参考にしてまとめていますが、内容に間違いがある可能性があります。  
ご注意ください。

## 設定ファイル

設定ファイルは `~/.lem/init.lisp` です。

## 関数を探す方法

### lisp-apropos

`M-x lisp-apropos` でキーワードを検索できます。  
`lisp-apropos` からは `q` キーで抜けられます。

### lisp-describe-symbol

`M-x lisp-describe-symbol` でリファレンスを参照できます。  
lisp-mode では `C-c C-d d` で `lisp-describe-symbol` を実行できます。

### Lem の実装を読む

- [ソースコード](https://github.com/cxxxr/lem/tree/master/lib)
- [キーワード検索](https://github.com/cxxxr/lem/search)

`lem-base` や `lem` パッケージの `export` された関数を調べる感じになります。

### Emacs の例を探す

Lem の関数と Emacs の関数は非常に似ているため、参考になります。  
Lem は Common Lisp なので名前空間を意識する必要があります。

### ドキュメントを参照する

- https://github.com/cxxxr/lem/blob/master/document/text.md

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

#### 現在のポイント情報を保存してから操作を行う

`save-excursion` を使用します。

````lisp
;; save-excursion
(lem-base:save-excursion
 (something-to-do))
````

### 関数の作例

#### 指定した位置にポイントを移動する

`move-to-position` で指定した位置にポイントを移動させることができます。
移動先はバッファの中の絶対位置で指定します。

````lisp
;; move-to-position を使ってポイントをバッファの先頭に移動する
(lem-base:move-to-position (lem-base:current-point) 1)
````

#### 文字数を指定してポイントを移動する

`forward-char` と `backward-char` でポイントを前後に移動します。
移動の量は文字数で指定します。

````lisp
(lem:forward-char 10)
(lem:backward-char 10)
````

#### 行を指定してポイントを移動する

`move-to-line` を使用して、指定した行数に移動します。

````lisp
;; move to 90th line
(lem-base:move-to-line (lem-base:current-point) 90)
````

#### 場所を指定してポイントを移動する

`line-start` や `buffer-end` など、場所を指定してポイントを移動させます。

````lisp
;; move to the beginning of current line
(lem-base:line-start (lem-base:current-point))

;; move to the end of current buffer
(lem-base:buffer-end (lem-base:current-point))
````

#### ポインタの位置を確認する

`position-at-point` や `line-number-at-point` でポイントの位置を確認します。

````lisp
;; check current position
(lem-base:position-at-point (lem-base:current-point))

;; check current line-number
(lem-base:line-number-at-point (lem-base:current-point))
````

#### ポイントの場所を確認する

`start-line-p` や `end-line-p` を使用して、ポイントの場所を確認します。

````lisp
;; check if the point is at the beginning of the line and the end of the line
(let ((point (lem-base:current-point)))
  (cons (lem-base:start-line-p point)
        (lem-base:end-line-p point)))
````

#### ポイントの位置にある文字を確認する

`character-at` でポイントの位置にある文字を確認します。

````lisp
;; check the char at the point
(lem-base:character-at (lem-base:current-point))
````

#### 文字を挿入する

`insert-char` でポイントの位置に文字を挿入します。

````lisp
;; insert "あ"
(lem-base:insert-character (lem-base:current-point) #\あ)

;; insert new line
(lem-base:insert-character (lem-base:current-point) #\newline)
````

#### 文字列を挿入する

文字列の挿入は `insert-string` を使用します。

ポイントの位置を保持する:
````lisp
;; insert-here
(defun insert-here (str)
  (lem-base:save-excursion
   (lem-base:insert-string (lem-base:current-point) str)))

(insert-here "foobarbaz")
````

ポイントの位置を挿入した文字の後ろに移動する:
````lisp
;; insert-here-and-move
(defun insert-here-and-move (str)
  (lem-base:insert-string (lem-base:current-point) str)) 

(insert-here-and-move "foobarbaz")
````

#### 現在のポイントの次の行に文字列を挿入する

`line-end` で行末に移動します。

ポイントの位置を保持する:
````lisp
;; insert-next-line
(defun insert-nextline (str)
  (lem-base:save-excursion
   (let ((point (lem:line-end (lem-base:current-point))))
     (lem-base:insert-character point #\newline)
     (lem-base:insert-string point str))))

(insert-nextline "foobarbaz")
````

ポイントの位置を挿入した文字の後ろに移動する:
````lisp
;; insert-next-line
(defun insert-nextline-and-move (str)
  (let ((point (lem:line-end (lem-base:current-point))))
    (lem-base:insert-character point #\newline)
    (lem-base:insert-string point str)))

(insert-nextline-and-move "foobarbaz")
````

#### ポイントの位置の文字を削除する

`delete-character` を使用してポイントの位置にある文字を削除します。

````lisp
;; delete a charcter
(lem-base:delete-character (lem-base:current-point))
````

#### ポイントの位置にマークを設定する

`set-current-mark` でマークを設定します。

````lisp
;; set mark on current point
(lem-base:set-current-mark (lem-base:current-point))
````

#### ポイントの複製

`copy-point` を使用してポイントを複製します。

````lisp
(let ((new (lem-base:copy-point (lem-base:current-point))))
  (lem-base:move-to-position new 1))
````



## バッファ

## ウィンドウ 

## モードライン

### バッファ名の取得

````lisp
(lem:modeline-name lem::*current-window*)
````

