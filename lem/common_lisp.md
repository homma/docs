## Commn Lisp プログラミング
- Lem 固有ではない Common Lisp のプログラミングについてまとめます

### S 式の自動整形

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

### 再帰関数を定義する

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

### 変数の型を確認する

````lisp
(type-of 'foo)
````
