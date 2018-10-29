Title: Lem のビルド方法

### Lem のビルド

Lem は初回起動時に自動でビルドが行われます。

````sh
$ lem
Making core for Roswell...
building dump:/Users/me/.roswell/impls/x86-64/darwin/sbcl-bin/1.4.12/dump/lem-ncurses.core
````

ビルドに使用するソースコードは `~/.roswell/local-projects/cxxxr/lem` から取得されます。  
このソースコードは `ros install cxxxr/lem` した際にインストールされたものです。  
`ros update lem` した際も、このソースコードが更新されます。

ビルドされたイメージは、`~/.roswell` 以下に出力され、2 回目以降の起動時にはこのイメージが使用されます。

````sh
~/.roswell/impls/x86-64/darwin/sbcl-bin/1.4.12/dump/lem-ncurses.core 
````

このイメージを削除してから Lem を起動すると、再びビルドが実行されます。

````sh
$ rm ~/.roswell/impls/x86-64/darwin/sbcl-bin/1.4.12/dump/lem-ncurses.core
$ lem
Making core for Roswell...
building dump:/Users/me/.roswell/impls/x86-64/darwin/sbcl-bin/1.4.12/dump/lem-ncurses.core
````

イメージの削除は `ros delete dump` コマンドでも実行できます。

````sh
$ ros delete dump lem-ncurses
````

ビルドは `ros build` コマンドで明示的に実行することも可能です。  
その場合は、`.ros` ファイルを指定する必要があります。

````sh
$ cd ~/.roswell/local-projects/cxxxr/lem
$ ros build roswell/lem-ncurses.ros
Making core for Roswell...
building dump:/Users/me/.roswell/impls/x86-64/darwin/sbcl-bin/1.4.12/dump/lem-ncurses.core
````

`ros dump executable` コマンドを使用して、実行ファイルを作成することも可能です。  
この場合も `.ros` ファイルを指定します。

````sh
$ ros dump executable roswell/lem-ncurses.ros -o /tmp/lem.bin
$ /tmp/lem.bin
````