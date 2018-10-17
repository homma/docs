## Commn Lisp プログラミング
- Lem 固有ではない Common Lisp のプログラミングについてまとめます

### プログラム

#### S 式の自動整形

- pprint を使用する
````lisp
(let ((*print-case* :downcase)) 
  (pprint '(labels ((f (n acc) (if (= 0 n) acc (f (- n 1) (cons n acc))))) (f 10 nil))))

;; output
(labels ((f (n acc)
           (if (= 0 n)
               acc
               (f (- n 1) (cons n acc)))))
  (f 10 nil))
````

### 関数

#### 再帰関数を定義する

- labels を使用する
````lisp
(labels ((f (n acc) 
            (if (= 0 n) 
                acc 
                (f (- n 1) (cons n acc))))) 
  (f 10 nil)) 
=> (1 2 3 4 5 6 7 8 9 10)
````

- let* は recursive ではなく sequential なので、再帰関数定義には使用できない
  - http://www.lispworks.com/documentation/lw60/CLHS/Body/s_let_l.htm

### リストの操作

#### リストに関数を適用した結果のリストを取得する

関数を適用した結果のリストを得たい場合は `mapcar` を使用します。

````lisp
(mapcar (lambda (n) (+ 1 n)) '(1 2 3 4 5))
;; => => (2 3 4 5 6)
````

#### リストの関数を適用し、結果は捨てる

関数の適用だけしたい場合は `mapc` を使用します。

````lisp
(mapc #'print '(1 2 3 4))
````

#### フィルタリングする

リストから指定した条件を満たす値を取り出したリストを作成するには `mapcan` を使用します。  
`mapcan` は第 1 引数の関数をリストに適用し、関数が返すリストを `nconc` でまとめます。

````lisp
(mapcan (lambda (n) (and (oddp n) (list n))) '(1 2 3 4 5))
;; => (1 3 5)
````

毎回 `list` せずに使いたい。

````lisp
(defun my-filter (f list)
  (mapcan (lambda (n) (and (funcall f n) (list n))) list))

(my-filter #'oddp '(1 2 3 4 5))
;; => (1 3 5)
````

### 文字列操作

#### 数値を文字列に変換する

`write-to-string` で変換します。

````lisp
(write-to-string 42)
````

#### リストを文字列に変換する

`write-to-string` で変換します。

````lisp
(write-to-string '(1 2 3))
````

### 変数

#### 変数の型を確認する

````lisp
(type-of 'foo)
````

### パッケージ

#### 使用可能なパッケージの一覧を確認する

`list-all-packages` で確認できます。

````lisp
(list-all-packages)
````

#### 現在使用しているパッケージの確認

`*package*` に現在使用しているパッケージ情報が格納されている。  
`package-name` でパッケージ情報からパッケージ名を文字列で取り出せる。

````lisp
;; check current package
(package-name *package*)
;; => "COMMON-LISP-USER"
````

#### パッケージを探す

`find-package` でパッケージ名（文字列）をパッケージ情報に変換できる。

````lisp
(find-package 'common-lisp-user)
;; => #<package "COMMON-LISP-USER">
````

#### 現在使用しているパッケージが use しているパッケージの一覧を確認する

````lisp
;; check package list
(mapcar #'package-name (package-use-list 'common-lisp-user))
;; => ("COMMON-LISP" "SB-ALIEN" "SB-DEBUG" "SB-EXT" "SB-GRAY" "SB-PROFILE")
````

#### シンボルが所属しているパッケージを確認する

````lisp
;; timer-name is in 'sb-ext
(symbol-package 'timer-name)
;; => #<package "SB-EXT">
````

#### パッケージの中のシンボルの属性を確認する

````lisp
;; timer-name is inherited
(find-symbol (string 'timer-name))
;; => timer-name, :inherited
````

### 時間

#### スリープする

````lisp
;; sleep
(progn
  (sleep 3)
  (get-decoded-time))
````

#### 時刻を確認する

````lisp
;; time components
(get-decoded-time)

;; timestamp
(get-universal-time)

;; display time info
(multiple-value-bind
      (sec min hour day month year dow dst tz)
    (get-decoded-time)
  (declare (ignore dow dst))
  (list year month day hour min sec tz))
````