
`:lua` は Lua のプログラムを実行する  
`:luado` はバッファ内の各行に対して Lua プログラムを実行する

# バッファの名前を取得する

現在のバッファ
````
lua print(vim.fn.bufname())
````

バッファ番号を指定
````
lua print(vim.fn.bufname(1))
````

# カーソルが位置する列を取得する

````
lua vim.fn.col('.')
````

# カーソルが位置する行を取得する

````
lua print(vim.fn.line('.'))
````

# カーソル行のバイト数を取得する

````
lua vim.fn.col('$')
````

# ビジュアルモードの開始位置を取得する

````
lua vim.fn.col('v')
````

# 選択ダイアログを表示する

戻り値はインデックスの数字

````
lua print(vim.fn.confirm('msg', 'yes\nno\ncanncel'))
````

# カーソルを移動する

n 行目の m バイト目に移動する場合
````
lua vm.fn.cursor(n, m)
````

# 環境変数を確認する

環境変数が存在するか確認する
````
lua print(vim.fn.has_key(vim.fn.environ(), 'HOME'))
````

値の取得
````
lua print(vim.fn.getenv('HOME'))
````

# 外部コマンドのパスを確認する

````
lua print(vim.fn.exepath('ls')) 
````

# バッファの情報を確認する

````
lua print(vim.fn.getbufinfo(1)[1].name)
````

# バッファ内の指定行の文字列を取得する

````
lua print(vim.fn.getbufline(1, 2, '$')[1])
````

バッファと行数を指定
````
lua print(vim.fn.getbufoneline(1, 2))
````

カレントバッファを対象とする
````
lua print(vim.fn.getline(10))
````

# バッファ変数を取得する

使用方法不明

````
lua print(vim.fn.getbufvar(1, '')[1])  
````

# コマンドラインでユーザの入力を 1 文字読み込む

````
lua print(vim.fn.getchar())
````

# コマンドラインでユーザの入力を一行読み込む
````
lua print(vim.fn.input('> '))
````

# コマンドラインの情報を取得する
````
vim.keymap.set('c', '<C-k>', function()
  local str = vim.fn.getcmdline()
  local pos = vim.fn.getcmdpos() - 1
  local substr = str:sub(1, pos)
  vim.fn.setcmdline(substr)
end, kopt)
````

# カーソルの位置を取得する

````
lua print(vim.fn.getcurpos()[3])
````

# カレントディレクトリを取得する

````
lua print(vim.fn.getcwd()) 
````

# ロードしたスクリプトの情報を取得する

````
lua print(vim.fn.getscriptinfo()[1].name)
````

# ウィンドウの情報を確認する

````
lua print(vim.fn.getwininfo()[1].winid)
````

# フィーチャーの確認

macOS 上で動作しているかの確認
````
lua print(vim.fn.has('mac'))
````

# キーマップが存在するかの確認

````
lua if(vim.fn.hasmapto('<C-e>', 'i') == 0) then print('yes') else print('no')
 end
````

# 指定した行のインデント量を確認する

以下は 46 行目のインデントのスペースの数を表示する

````
lua print(vim.fn.indent(46))
````

# シェルのコマンドを実行する

````
lua vim.fn.jobstart('echo foo')
````


# バッファの名前を取得する

現在のバッファ
````
lua print(vim.fn.bufname())
````

バッファ番号を指定
````
lua print(vim.fn.bufname(1))
````

# カーソルが位置する列を取得する

````
vim.fn.col('.')
````

# カーソル行のバイト数を取得する

````
vim.fn.col('$')
````

# ビジュアルモードの開始位置を取得する

````
vim.fn.col('v')
````

# 選択ダイアログを表示する

戻り値はインデックスの数字

````
lua print(vim.fn.confirm('msg', 'yes\nno\ncanncel'))
````

# カーソルを移動する

n 行目の m バイト目に移動する場合
````
lua vm.fn.cursor(n, m)
````

# 環境変数を確認する

環境変数が存在するか確認する
````
lua print(vim.fn.has_key(vim.fn.environ(), 'HOME'))
````

値の取得
````
lua print(vim.fn.getenv('HOME'))
````

# 外部コマンドのパスを確認する

````
lua print(vim.fn.exepath('ls')) 
````

# バッファの情報を確認する

````
lua print(vim.fn.getbufinfo(1)[1].name)
````

# バッファ内の指定行の文字列を取得する

````
lua print(vim.fn.getbufline(1, 2, '$')[1])
````

バッファと行数を指定
````
lua print(vim.fn.getbufoneline(1, 2))
````

カレントバッファを対象とする
````
lua print(vim.fn.getline(10))
````

# バッファ変数を取得する

使用方法不明

````
lua print(vim.fn.getbufvar(1, '')[1])  
````

# コマンドラインでユーザの入力を 1 文字読み込む

````
lua print(vim.fn.getchar())
````

# コマンドラインでユーザの入力を一行読み込む
````
lua print(vim.fn.input('> '))
````

# コマンドラインの情報を取得する
````
vim.keymap.set('c', '<C-k>', function()
  local str = vim.fn.getcmdline()
  local pos = vim.fn.getcmdpos() - 1
  local substr = str:sub(1, pos)
  vim.fn.setcmdline(substr)
end, kopt)
````

# カーソルの位置を取得する

````
lua print(vim.fn.getcurpos()[3])
````

# カレントディレクトリを取得する

````
lua print(vim.fn.getcwd()) 
````

# ロードしたスクリプトの情報を取得する

````
lua print(vim.fn.getscriptinfo()[1].name)
````

# ウィンドウの情報を確認する

````
lua print(vim.fn.getwininfo()[1].winid)
````

# フィーチャーの確認

macOS 上で動作しているかの確認
````
lua print(vim.fn.has('mac'))
````

# キーマップが存在するかの確認

````
lua if(vim.fn.hasmapto('<C-e>', 'i') == 0) then print('yes') else print('no')
 end
````

# 指定した行のインデント量を確認する

以下は 46 行目のインデントのスペースの数を表示する

````
lua print(vim.fn.indent(46))
````

# シェルのコマンドを実行する

`vim.system()` も `vim.fn.jobstart()` も上手く動作せず。  
要調査。  

`vim.fn.system` は動作した。

````
lua print(vim.fn.system('ls /'))
````

`systemlist` という API もある。

--------------------------------------------------------------------------------

maparg から先は飛ばし読み
重要そうなのは以下

keymap 関連の map~
screen 上の位置に関する screen~
server~
setbufline
setline
setcmd~
setcursorcharpos
setpos
strdisplaywidth
strwidth
termopen
timer
visualmode
win_getid
wincol
winheight
winline
winnr
winwidth

--------------------------------------------------------------------------------

visualmode で現在選択されている文字列を取得する API はない

