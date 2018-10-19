## Lem からプログラムを実行する

`uiop` を使用してプログラムを実行し、サブプロセスとやり取りをする方法をまとめます。

### コマンドを実行し、その出力を取得する

````lisp
(let ((p (uiop:launch-program "echo foo" :input :stream :output :stream)))
  (print (read-line (uiop:process-info-output p))))
````

### コマンドを実行し、入力を与え、出力を受け取る

````lisp
(defun listen-with-timeout (timeout interval in)
  (if (< timeout 0)
      nil
      (if (listen in)
          t
          (progn
            (sleep interval)
            (listen-with-timeout (- timeout interval) interval in)))))

(defun read-all (in)
  (labels ((%read-all (acc in)
             (let ((timeout 0.01)
                   (interval 0.01))
               (if (listen-with-timeout timeout interval in)
                   (%read-all (cons (read-line in) acc) in)
                   acc))))
    (%read-all nil in)))

(defun write-and-flush (str out)
  (progn
    (write-line str out)
    (finish-output out)))

(defun write-buffer (str)
  (let ((point lem-base:current-point))
    (lem-base:insert-character point #\newline)
    (lem-base:insert-string point str)))

(let* ((stdout *standard-output*)
       (p (uiop:launch-program "bc -lq"
                               :input :stream
                               :output :stream))
       (p-in (uiop:process-info-input p))
       (p-out (uiop:process-info-output p)))
  (write-and-flush "1 * 2 * 3 * 4" p-in)
  (print "foo" stdout)
  (finish-output stdout)
  (print (read-all p-out) stdout)
  (finish-output stdout)
  (write-and-flush "10+10" p-in)
  (print (read-all p-out) stdout)
  (finish-output stdout)
  (write-and-flush "quit" p-in)
  (uiop:close-streams p)
  (uiop:wait-process p))
````

## メモ

### print

(prog ()
  (print "foo")
  (sleep 3))

(prog ()
  (print "foo" *standard-output*))

(let* ((stdout *standard-output*))
  (print "foobar" stdout)
  (finish-output stdout)
  (sleep 3))

((lambda ()
   (prog ()
     (print "foo" *standard-output*)
     (finish-output *standard-output*)
     (sleep 3))))

### launch-program
````lisp
(let* ((p (uiop:launch-program "bc -l"
                               :input :stream
                               :output :stream))
       (p-in (uiop:process-info-input p))
       (p-out (uiop:process-info-output p)))
  (write-line "3 * 3 + 4" p-in)
  (finish-output p-in)
  ;; (sleep 1)
  (sleep 0.01)
  (listen p-out))
  ;; (read-char-no-hang p-out))
  ;;(list (read-char p-out) (read-char-no-hang p-out)))
  ;; (list (read-char-no-hang p-out) (read-char-no-hang p-out)))
  ;; (stream-read-char-no-hang p-out))
  ;; (list (listen p-out) (listen p-out) (listen p-out)))
  ;; (read-char p-out))
  ;; (stream-read-char-no-hang p-out))
  ;; (read-char-no-hang p-out))
  ;; (type-of p-out)) ;; => sb-sys:fd-stream
  ;; (read-line p-out))
  ;; (listen p-out))
  (labels ((read-all (acc in)
             (if (listen in)
                 (let ((d (read-line in)))
                   (print "got")
                   (print d)
                   (read-all (cons d acc) in))
                 (progn
                   (print "not")
                   acc))))
    (read-all nil p-out)))
````

