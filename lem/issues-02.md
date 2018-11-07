## 問題点

- バッファから評価すると遅い
- Lem 内の Lisp REPL から実行しても遅い
- OS コマンドをリストで渡すと速い
- Lem を使用せず、SBCL で実行すると速い

## 状況

- コマンドを文字列で渡すと遅い
- コマンドをリストで渡すと速い

- 遅いのは、
  - バッファ上の S 式を `C-x C-e` で評価した場合
  - `start-lisp-repl` で REPL を起動して評価した場合

- `:force-shell t` オプションを設定すると、コマンドをリストで渡しても遅い

- Lem を使用せず、`ros run -e` で実行すると、コマンドを文字列で渡しても速い

## 予想
- `wait-process` が遅いのかもしれない

## 実装
- [run-program.lisp](https://github.com/fare/asdf/blob/master/uiop/run-program.lisp)
- [launch-program.lisp](https://github.com/fare/asdf/blob/master/uiop/launch-program.lisp)

### run-program.lisp
- 遅いケースでは `%use-system` が実行される
  - コマンドが文字列で渡された
  - `:force-shell t` オプションが設定された

- 速いケースでは、`%use-launch-program` が実行される

### %use-system
- `%system` を呼んでいる
- `%system` は `launch-program` を呼んでいる
- `wait-process` で結果を待っている

### %use-launch-program
- `launch-program` を呼んでいる

### launch-program.lisp

## テスト結果

### コマンドを文字列で渡すと遅い

````lisp
(time (uiop:run-program "date" :output :string))
;;  1.008 seconds of real time
````

### コマンドをリストで渡すと速い
````lisp
(time (uiop:run-program '("date") :output :string))
;;  0.005 seconds of real time
````

### `start-lisp-repl` の REPL から実行しても遅い

````lisp
cl-user> (time (uiop:run-program "date" :output :string)) 
;;  1.004 seconds of real time
````

### `start-lisp-repl` の REPL から実行しても速い:
````lisp
cl-user> (time (uiop:run-program '("date") :output :string))
;;  0.003 seconds of real time
````

### `:force-shell t` を付けると遅い

````lisp
(uiop:run-program '("date") :output :string :force-shell t)
;;   1.008 seconds of real time
````

### Lem を経由しないで実行すると速い
````lisp
% ros run -e '(time (uiop:run-program "date" :output :string))(quit)'
;;  0.006 seconds of real time
````

