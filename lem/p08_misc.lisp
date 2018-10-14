
## その他

### ボタン

使い方がよくわからない。

````lisp
(lem-base:with-point ((point (lem-base:current-point)))
  (lem.button:insert-button
   point
   "My Button"
   (lambda () (lem-base:insert-string point "clicked!"))))
````