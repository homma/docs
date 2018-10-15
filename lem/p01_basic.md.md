<!-- (lem-lisp-mode:lisp-mode)
-->

## Lem の基本的な操作

Lem の画面上でキーボードを使用して操作するのと同じ処理をプログラムから実行する方法をまとめます。

扱っている関数の実装は主に以下のファイルにあります。

- [command.lisp](https://github.com/cxxxr/lem/blob/master/lib/core/command.lisp)
- [help-command.lisp](https://github.com/cxxxr/lem/blob/master/lib/core/help-command.lisp)
- [file-command.lisp](https://github.com/cxxxr/lem/blob/master/lib/core/file-command.lisp)
- [word-command.lisp](https://github.com/cxxxr/lem/blob/master/lib/core/word-command.lisp)
- [window-command.lisp](https://github.com/cxxxr/lem/blob/master/lib/core/window-command.lisp)

## Lem のプログラムの実行方法

Lem のバッファに Lisp プログラムを記述すると、エディタ上から直接そのプログラムを実行することができます。  

プログラムの実行は、最後の閉じ括弧の後ろで `C-x C-e` を入力します。
通常、実行結果はミニバッファに出力されます。

このドキュメントを Lem のバッファにピーし、以下の場所で `C-x C-e` を入力してください。

````lisp
;; sample Lisp program
((lambda (x) x)  "this is a Lisp program")
;;                                        ~~~ ここで C-x C-e
````

ミニバッファに "this is a Lisp program" と出力されます。

`M-x lisp-mode` を実行して lisp-mode を使用することで、より便利に Lisp プログラムを操作することができます。

## パッケージ

Lem は Common Lisp で作成されているため、関数はパッケージに所属しています。  
今回使用するパッケージは `lem` パッケージです。
`lem` パッケージの関数を使用する際は、関数名の前に `lem:` を追加します。

````lisp
;; calling with `lem:` prefix
(lem:lem-version)
````

まとまったプログラムを書く場合は、`defpackage` を使用して独自のパッケージを作成します。  
パッケージを作成する際に、別のパッケージを `use` すると、そのパッケージに含まれる関数はパッケージ名なしで呼び出すことができます。

````lisp
(defpackage my-package (:use cl lem))
(in-package my-package) ;; can be omitted

(lem-version)
````

このドキュメントでは小さなプログラムしか扱わないのと、どのパッケージの関数かを明示するために、常にパッケージ名を使用して関数を呼び出すことにします。

それでは、ここからは具体的なプログラムの例を見て行きます。

## プログラムの作例

### Lem を終了する

`exit-lem` 関数を使用して Lem を終了します。

````lisp
;; exit from the editor
(lem:exit-lem)
````

未保存のデータがある場合は、保存を行うか確認されます。  
未保存のデータがない場合は、エディタが終了します。

`quick-exit` は保存の確認を行わずに終了します。

````lisp
;; exit withour confirmation
(lem:quick-exit)  
````

### ポイント（カーソル）を文字単位で移動する

`forward-char` と `backward-char` はポイントを文字単位で移動します。
移動回数は文字数で指定します。

````lisp
;; move forward
(lem:forward-char 10)

;; move backward
(lem:backward-char 10)
````

### ポイント（カーソル）を行単位で移動する

`previous-line` と `next-line` はポイントを行単位で移動します。
移動回数は行数で指定します。

````lisp
;; previous line
(lem:previous-line 10)

;; next line
(lem:next-line 10)
````

`goto-line` は指定した行番号にカーソルを移動します。

````lisp
(lem:goto-line 1)
````

### 改行する

`newline` はカーソル位置に改行を追加します。

````lisp
;; insert a #\newline
(lem:newline)
````

`open-line` を使用すると指定した数の改行を追加できます。

````lisp
(lem:open-line 5)
````

### 文字を削除する

`delete-previous-char` と `delete-next-char` は指定した数の文字を削除します。

````lisp
;; delete preceeding n characters
(lem:delete-previous-char 10)

;; delete following n characters
(lem:delete-next-char 10);;;;;;;;;;
````

### 空行を削除する

`delete-blank-lines` は後続の空白行を削除します。

````lisp
;; delete following blank lines
(lem:delete-blank-lines)




````

### バッファの名前を変更する

`rename-buffer` でバッファの名前を変更できます。

````lisp
;; change buffer name
(lem:rename-buffer "another-name")
````

### バッファを読み取り専用にする

`toggle-read-only` を実行するとバッファが読み取り専用に設定され、バッファに対する変更ができなくなります。  
バッファが読み取り専用になると、モードラインの表示が `%` になります。

````lisp
(lem:toggle-read-only)
````

再び書き込み可能にするには、再度 `toggle-read-only` を実行するか、`C-x C-q` を入力します。

## その他の例

### raw input を読み込んで出力する

`quote-insert` を使用すると `Ctrl-v` を入力した時のように、raw input を読んで出力することができます。

````lisp
;; read raw input and write it
(lem:quoted-insert)
````

## 注記

`use-package` はエラーが発生するため使いません。  
本来的には独自のパッケージを定義するべきですが、ここで紹介しているコード片ごとにパッケージ定義を行うのも大変なので、完全修飾した関数名を使用します。  

完全修飾の場合はどのパッケージに含まれているか明確化されるというメリットもあります。

````lisp
;; [fail]
;; cannot include lem package when we are in common-lisp-user package
(use-package 'lem)
;; => name conflict
;; => lem:timer-name, sb-ext:timer-name

;; [OK]
;; this succeeds
(defpackage my-package (:use cl lem))
(in-package my-package) ;; can be omitted
(lem-version)
;; => should evaluate for each line

;; [fail]
;; lem-version is evaluated at compile time into common-lisp-user package
;; import is only a function that is  evaluated at run time
;; where lem-version is already exists
(progn
  (import '(lem:lem-version))
  (lem-version))
;; => name conflict
;; => lem:lem-version, common-lisp-user::lem-version

;; [OK]
;; this succeeds
(import '(lem:lem-version)
(lem-version)
;; => should evaluate for each line

;; check current package
*package*
````
