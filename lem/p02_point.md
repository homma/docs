<!-- (lem-lisp-mode:lisp-mode)
-->

## ポイント

ポイント（カーソル位置の情報）を使用したプログラムの書き方をまとめます。  

前回もポイントを操作する関数がありました。  
今回は `current-point` を使用して明示的にポイントのデータを操作します。  
また、新たにポイントを作成（複製）して、そのポイントに対して操作を行います。

扱っている関数の実装は主に以下のファイルにあります。

- [basic.lisp](https://github.com/cxxxr/lem/blob/master/lib/base/basic.lisp)
- [point.lisp](https://github.com/cxxxr/lem/blob/master/lib/base/point.lisp)

### 基本的な関数とマクロ

#### 現在のポイントを取得する

`current-point` で現在のポイントを取得できます。

````lisp
;; current-point
(let ((point (lem-base:current-point)))
  (do-something-with-point))
````

#### ポイントの複製

`copy-point` を使用してポイントを複製します。

````lisp
;; copy-point
(let ((new (lem-base:copy-point (lem-base:current-point))))
  (do-something-with-new-point)
  (lem-base:delete-point new))
````

`copy-point` を使用してポイントを複製した際に、`:temporary` をつけて一時的な読み取りポイントを作成しなかった場合は、`delete-point` を使って明示的にポイントを削除する必要があります。

`with-point` マクロを使用すると、コピーしたポイントを `delete-point` を使用して自動的に削除してくれます。

````lisp
;; with-point
(with-point ((new (lem-base:current-point)))
  (do-somthing-with-new-point))
````

#### 現在のポイント情報を保存してから操作を行う

`save-excursion` マクロを使用します。

````lisp
;; save-excursion
(lem-base:save-excursion
  (do-something-1)
  (do-something-2))
````

### 関数の作例

#### ポイントの位置を確認する

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

#### 指定した位置にポイントを移動する

`move-to-position` で指定した位置にポイントを移動させることができます。
移動先はバッファの中の絶対位置で指定します。

````lisp
;; move to the beginnig of current buffer
(lem-base:move-to-position (lem-base:current-point) 1)
````

#### 移動する量を指定してポイントを移動する

`character-offset` や `line-offset` を使用してポイントを移動します。  
引数には移動させたいポイントと移動させる量を指定します。  
移動する量がマイナスの場合は前方に移動します。

````lisp
;; character-offset
(let ((point (lem-base:current-point)))
  (lem-base:character-offset point 10))

;; line-offset
(let ((point (lem-base:current-point)))
  (lem-base:line-offset point -1))
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

#### ポイントの位置にある文字を確認する

`character-at` でポイントの位置にある文字を取得します。

````lisp
;; check the char at the point
(lem-base:character-at (lem-base:current-point))
````

#### ポイントのある行の文字列を確認する

`line-string` でポイントのある行の文字列を取得します。

````lisp
;; obtain string of the point
(lem-base:line-string (lem-base:current-point))
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

#### ポイントの間の文字列を削除する

`delete-between-points` を使用して二つのポイントの間の文字列を削除します。

````lisp
;; remove string between points
(let ((start (lem-base:current-point)))
  (lem-base:with-point ((end start))
    (lem-base:line-offset end 1)
    (lem-base:line-end end)
    (lem-base:delete-between-points start end)))
;; remove this line ............................
````

上記は `with-point` を使用してポイントを複製しています。

#### ポイントの位置にマークを設定する

`set-current-mark` でマークを設定します。

````lisp
;; set mark on current point
(lem-base:set-current-mark (lem-base:current-point))
````