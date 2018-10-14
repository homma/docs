<!-- (lem-lisp-mode:lisp-mode)
-->

## モード

- [mode.lisp](https://github.com/cxxxr/lem/blob/master/lib/core/mode.lisp)
- [modeline.lisp](https://github.com/cxxxr/lem/blob/master/lib/core/modeline.lisp)

- [file-ext.lisp](https://github.com/cxxxr/lem/blob/master/lib/core/file-ext.lisp)

### バッファ名の取得

````lisp
(lem:modeline-name lem::*current-window*)
````

### ファイルタイプとメジャーモードの関連付け

`lem:*auto-mode-alist*` に関連付けが格納されています。

````lisp
;; dump *auto-mode-alist*
;; to be fixed
(let ((list lem:*auto-mode-alist*)
      (point (lem-base:current-point)))
  (labels ((display (str)
             (lem-base:insert-character point #\newline)
             (lem-base:insert-string point str)))
    (dolist (i list)
      (display (car i))
      (display (cdr i)))))

;; type
(type-of lem:*auto-mode-alist*)
(type-of lem-diff-mode:diff-mode)
(type-of lem-scheme-mode:scheme-mode)
````
