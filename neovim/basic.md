
# NeoVim

## ファイルを開く

````
:e ~/path/to/file
````

## コピーアンドペースト

OS のクリップボードとの連携
y でクリップボードにコピー、p でクリップボードからペースト

````
vim.opt.clipboard = "unnamedplus"
````

## ウィンドウ分割

縦分割

````
Ctrl-w v
````

````
:vnew
````

現在開いているファイルで新しいウィンドウを作成する

````
:vsp
````

新しいウィンドウを右側に作成する

````
vim.opt.splitright = true
````

横分割

````
Ctrl-w s
````

````
:new
````

現在開いているファイルで新しいウィンドウを作成する

````
:sp
````

ウィンドウ間の移動

````
Ctrl-w Ctrl-w
````

## タブの作成

````
:newtab
````

タブ間の移動

````
gt
````

````
gT
````

## バッファ

バッファへのアクセス

````
:buffers
````

````
:buffer 1
````

## 設定ファイル

````
$ mkdir -p ~/.config/nvim
$ vi ~/.config/nvim/init.lua
````

## Lua モジュール

````
$ mkdir -p ~/.config/nvim/lua
$ mkdir -p ~/.config/nvim/lua/module_name
````

# NeoVim Qt

## フォントの設定

現在設定されているフォントの確認

````
:GuiFont
Courier New:h11
````

フォント設定パネルの表示

````
:GuiFont *
````

https://github.com/equalsraf/neovim-qt/issues/584

フォントの設定

````
:GuiFont Menlo:h12
````

````
:set guifont=Menlo:h12
````

````
vim.opt.guifont = "Menlo:h12"
````

## カラースキームの設定

````
:colorscheme desert
````

設定ファイル

````
vim.api.nvim_command [[colorscheme desert]]
````

