## Lem の基本的な操作

Lem の画面上でキーボードを使用して操作するのと同じ処理をプログラムから実行する方法をまとめます。

扱っている関数の実装は主に以下のファイルにあります。

- [command.lisp](https://github.com/cxxxr/lem/blob/master/lib/core/command.lisp)

## パッケージ

Lem は Common Lisp で作成されているため、関数はパッケージに所属しています。  
今回使用するパッケージは `lem` パッケージです。
`lem` パッケージの関数を使用する際は、関数名の前に `lem:` を追加します。

````lisp
;; calling with `lem:` prefix
(lem:lem-version)
````

Lisp プログラムは、最後の閉じ括弧の後ろで `C-x C-e` を入力することで実行できます。  
通常、実行結果はミニバッファに出力されます。

ここからは具体的なプログラムの例を見て行きます。

## プログラムの作例

#### Lem を終了する

`exit-lem` 関数を使用して Lem を終了します。

````lisp
(lem:exit-lem)
````

未保存のデータがある場合は、保存を行うか確認されます。  
未保存のデータがない場合は、エディタが終了します。

#### 移動する量を指定してポイントを移動する

`forward-char` と `backward-char` でポイントを前後に移動します。
移動の量は文字数で指定します。

````lisp
;; forward-char
(lem:forward-char 10)

;; backward-char
(lem:backward-char 10)
````

`previous-line` と `next-lie` でポイントを他の行へ移動させます。
移動の量は行数で指定します。

````lisp
;; previous-line
(lem:previous-line 1)

;; next-line
(lem:next-line 1)
````
