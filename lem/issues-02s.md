Title: additional latency on executing uiop:run-program on macOS

## Summary

There is an additional latency observed on executing `uiop:run-program` inside Lem on macOS.
I do not know why, but somehow it wastes one second for doing nothing when the function is called from Lem.

I have found that the problem is observed when the command argument is passed as a string.
And it can be avoided by passing the command argument as a list.

It would be helpful if you would consider passing commands as a list in `lib/core/command.lisp`.
It will speed up `filter-buffer` and `pipe-command` commands on macOS.

Also, the following issue may be related to the above behavior.

https://github.com/cxxxr/lem/issues/64

The root cause is unknown so far.

## In Japanese

macOS 上の Lem から `uiop:run-program` を呼び出すと、なぜか 1 秒間の遅延が追加されるようです。  

遅延が発生するのは、`uiop:run-program` に引数として渡されたコマンドが文字列の場合です。  
コマンドをリストにして渡した場合は、遅延は発生しません。

Lem の `filter-buffer` や `pipe-command` などはこの影響を受けており、macOS で実行すると余計な時間がかかります。  
内部で使用されている `uiop:run-program` の引数を文字列からリストに変更すると、遅延は解消されます。

原因は確認できていませんが、もし修正できるようでしたら、Lem の実装に使用されている `uiop:run-program` 関数の引数をリストに変更ください。

以下の Issue で報告されている遅延もこれが原因かもしれません。

https://github.com/cxxxr/lem/issues/64

## Details

When the following program is executed inside Lem on macOS, it takes additional one second before it returns.

```lisp
(time (uiop:run-program "date" :output :string))
;;  1.008 seconds of real time
;; slow!
````

On the other hand, when the same program is executed outside of Lem, it finishes quickly as expected in a few miliseconds.  

```lisp
% ros run -e '(time (uiop:run-program "date" :output :string))(quit)'
;;  0.006 seconds of real time
;; fast!
```

It seems that the problem occurs when it is called inside Lem.

The latency can be avoided by passing the command as list too.  

````lisp
(time (uiop:run-program '("date") :output :string))
;;  0.005 seconds of real time
;; fast! because the commands are passed as a list
;; and it usess `%use-launch-program`
````

This happens since it calls `%use-launch-program` inside `uiop:run-program`.

- https://github.com/fare/asdf/blob/master/uiop/run-program.lisp

When the commands are passed as string, `%use-system` is used instead of `%use-launch-program`.
It seems this makes it slow to execute the `uiop:run-program`.

Another case which calls `%use-system` is  adding `:force-shell` keyword to the `uiop:run-program` call.
It introduces a latency even if the commands are passed as a list.

````lisp
(uiop:run-program '("date") :output :string :force-shell t)
;;   1.008 seconds of real time
;; slow! because it uses `%use-system`
````

As an additional information, it is also slow when `uiop:run-program` is called from the REPL started by `start-lisp-repl`.

````lisp
cl-user> (time (uiop:run-program "date" :output :string)) 
;;  1.004 seconds of real time
;; slow!
````

So, it may be the swank which is introducing the latency.  
But I do not know how to test it.

## Other Info

All the above test has been done on macOS.
The latency has not been seen on Linux.
