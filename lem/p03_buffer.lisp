
## バッファ

バッファを取り扱うプログラムの書き方をまとめます。

呼び出している関数の実装は主に以下にあります。

- [buffer.lisp](https://github.com/cxxxr/lem/blob/master/lib/base/buffer.lisp)
- [buffers.lisp](https://github.com/cxxxr/lem/blob/master/lib/base/buffers.lisp)

### 現在操作しているバッファの取得

`current-buffer` で現在操作しているバッファを取得できます。

````lisp
;; obtain current buffer data
(lem-base:current-buffer)
````

### バッファの情報の取得

#### バッファ名

`buffer-name` でバッファ名を取得します。

````lisp
;; obtain current buffer name
(lem-base:buffer-name (lem-base:current-buffer))
````

#### ファイル名

`buffer-filename` でファイル名を取得します。

````lisp
;; obtain the filename of current buffer
(lem-base:buffer-filename (lem-base:current-buffer))
````

#### バッファの行数

`buffer-nlines` でバッファの行数を確認できます。

````lisp
(lem-base:buffer-nlines (lem-base:current-buffer))
````

### バッファの一覧の取得
````lisp
(lem-base:buffer-list)
````

