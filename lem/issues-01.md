## 問題点

1. 先頭に改行が入らない
2. 改行時に出力位置がリセットされない
3. もともと存在していたテキストが表示されたままになるので、出力が混ざる

## 調査方法
- `C-x C-e` をしている場所を調べて動作の仕組みを確認する

https://github.com/cxxxr/lem/blob/master/modes/lisp-mode/lisp-mode.lisp#L386
````lisp
      (if p
          ;; C-c C-e
          (self-eval-print string (- (window-width (current-window)) 2))
          ;; C-x C-e
          (self-interactive-eval string)))))
````

