
### インストール

````sh
$ brew install tmux
==> Installing dependencies for tmux: libevent
...
````

### 設定ファイル

`~/.tmux.conf`

#### サンプル設定ファイル

- /usr/local/opt/tmux/share/tmux/example_tmux.conf                                                                                                                  

### キーバインド

#### プリフィックスキー

- `Alt+n` にしました

~/.tmux.conf:
````
set -g prefix M-n
unbind C-b
````

#### キーバインドの一覧

`Prefix + ?`

### ウィンドウ操作

#### ウィンドウ作成

`Prefix + c`

create かな。

#### ウィンドウの横分割

`Prefix + %`

#### ウィンドウの縦分割

`Prefix + "`

#### ウィンドウの一覧確認

`Prefix + w`

#### ウィンドウを縦に均等な幅で並び替える

`Prefix + Alt 1` : horizontal

#### ウィンドウを横に均等な幅で並び替える

`Prefix + Alt 2` : vertical

### ウィンドウのレイアウトを変更する

`Prefix + Alt 3` : `T` 字型 (main-horizontal)  
`Prefix + Alt 4` : `E` 字型 (main-vertical)  
`Prefix + Alt 5` : `田` 字型 (tiled)  

#### ウィンドウを一つにする

`Prefix + !`

カーソルがあるウィンドウだけが残る。

#### ウィンドウの削除

`Prefix + x`

シェルセッションが終了する。　

#### ウィンドウ間の移動

`Prefix + Arrow`

矢印キーの方向に移動。

#### ウィンドウの入れ替え

`Prefix + {`  
`Prefix + }`

### コピーモード

#### コピーモード開始

`Prefix + [`

#### 範囲選択開始

`Ctrl-@`

#### 範囲選択終了

`Ctrl-w`

#### ペースト

`Prefix + ]`
