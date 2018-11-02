## 問題点

- バッファから評価すると遅い
- Lem 内の Lisp REPL から実行しても遅い
- OS コマンドをリストで渡すと速い
- Lem を使用せず、SBCL で実行すると速い

## 状況

遅い:
````lisp
(time (uiop:run-program "date" :output :string))
;; Evaluation took: 1.008 seconds of real time
````

`start-lisp-repl` の REPL から実行しても遅い
````lisp
cl-user> (time (uiop:run-program "date" :output :string)) 
Evaluation took:
  1.004 seconds of real time
  0.006484 seconds of total run time (0.002302 user, 0.004182 system)
  0.60% CPU
  4,025,911,340 processor cycles
  149,760 bytes consed

"2018年 11月 2日 金曜日 18時11分38秒 JST
"
nil
0
````

コマンドをリストで渡すと速い:
````lisp
(time (uiop:run-program '("date") :output :string))
;; Evaluation took: 0.005 seconds of real time
````

`start-lisp-repl` の REPL から実行しても速い:
````lisp
cl-user> (time (uiop:run-program '("date") :output :string))
Evaluation took:
  0.003 seconds of real time
  0.001056 seconds of total run time (0.000192 user, 0.000864 system)
  33.33% CPU
  13,778,526 processor cycles
  31,696 bytes consed

"2018年 11月 2日 金曜日 18時12分40秒 JST
"
nil
0
````

Lem を経由しないで実行すると速い:
````lisp
% ros run -e '(time (uiop:run-program "date" :output :string))(quit)'
Evaluation took:
  0.006 seconds of real time
  0.001842 seconds of total run time (0.000349 user, 0.001493 system)
  33.33% CPU
  22,144,553 processor cycles
  59,904 bytes consed
````

