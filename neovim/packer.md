
Lua で書かれており、使用者も多そうなので、Packer を使用する

- https://github.com/wbthomason/packer.nvim

パッケージの手動インストールは、Vim パッケージに慣れていないので控えたい

# インストール

Readme の通りに実行
````
% git clone --depth 1 https://github.com/wbthomason/packer.nvim ~/.local/share/nvim/site/pack/packer/start/packer.nvim
````

途中のディレクトリは事前に作成していなくても問題ない
インストール結果はこんなかんじ

````
% /bin/ls .local/share/nvim/site/pack/packer/start/packer.nvim/
Dockerfile      Makefile        doc             selene.toml     tests
LICENSE         README.md       lua             stylua.toml     vim.toml
````

Packer の設定スクリプト作成

````
% mkdir ~/.config/nvim/lua
% vi ~/.config/nvim/lua/plugins.lua
````

plugins.lua
````
-- load Packer
vim.cmd [[packadd "packer.nvim"]]

require"packer".startup(function()
  use "wbthomason/packer.nvim"
end)
````

init.lua に以下を追加

````
require "plugins"
````

nvim で以下を実行

````
:PackerInstall
:PackerCompile
````

# パッケージの追加

reorepl を追加してみる
- https://github.com/ii14/neorepl.nvim

plugins.lua
````
-- load Packer
vim.cmd [[packadd "packer.nvim"]]

require"packer".startup(function()
  use "wbthomason/packer.nvim"
  use "ii14/neorepl.nvim"
end)
````

nvim で以下を実行

````
:PackerInstall
:PackerCompile
````

動作を確認

````
:Repl
````

インストール先

````
~/.local/share/nvim/site/pack/packer/start
````

# パッケージの削除

plugins.lua からパッケージの記述を削除

````
:PackerClean
````

