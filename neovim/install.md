
結局マニュアルインストールすることにしました

# パッケージインストール 

````
% brew install neovim
````

# 最新版のインストール

以下は libvterm のエラーにより失敗

````
% brew install neovim --HEAD
````

# GUI

## Qt
neovim-qt は以下の問題があるためインストールしない

- 日本語入力中の文字が小さく表示される

ターミナルで実行する形で十分

# マニュアルインストール

- https://github.com/neovim/neovim/wiki/Installing-neovim
````
$ mkdir -p Applications/Neovim; cd $_
$ curl -LO https://github.com/neovim/neovim/releases/download/nightly/nvim-macos.tar.gz
$ tar xzf nvim-macos.tar.gz
$ ./nvim-macos/bin/nvim
````

アップデートは都度実行する必要あり

