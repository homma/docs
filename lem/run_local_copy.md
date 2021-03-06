Title: git clone した Lem を実行する

### git clone した Lem を実行する方法

以下のコマンドで GitHub から Lem を `git clone` して、実行することができます。

````lisp
$ git clone https://github.com/cxxxr/lem.git
$ cd lem
$ ros run -e '(setf ql:*local-project-directories* (list #p"."))(ql:quickload :lem-ncurses)(lem:lem)(quit)'
```` 

初回の実行時はコンパイルが行われるため、起動に少し時間がかかります。  
コンパイルされたプログラムは `~/.cache/common-lisp` にキャッシュされるため、次回の起動時からは比較的早く立ち上がります。

Lem のソースコードを編集して起動し直すと、自動で再コンパイルが実行されます。

Lem 自身のソースコードを弄りたい場合に、Roswell でインストールした Lem とは別に、git でクローンした開発用の Lem を保持することができるので便利です。

### 参考
- [The Quicklisp local-projects mechanism](http://blog.quicklisp.org/2018/01/the-quicklisp-local-projects-mechanism.html)
