
## Godot インストール
- https://godotengine.org/download/archive/

Godot archive から最新バージョンをダウンロード  
適当なディレクトリにコピーする

## dotnet SDK インストール

Godot で C# を使用するために必要

````sh
$ cd
$ mkdir <install dir>/.dotnet
$ ln -s <install dir>/.dotnet .
$ tar xf dotnet-sdk-8.0.100-osx-arm64.tar.gz -C ~/.dotnet
$ vi .zshrc
PATH=${HOME}/.dotnet:${PATH}

export DOTNET_CLI_TELEMETRY_OPTOUT=1
export DOTNET_ROOT=~/.dotnet
````

`Homebrew` の `dotnet-sdk` は `/etc` にファイルをインストールしようとするのでやめました

## dotnet setup for Godot

### 問題
- https://github.com/godotengine/godot/issues/75668
- https://github.com/godotengine/godot/pull/74221

Godot のエディタを Finder からダブルクリックで起動するためには、dotnet が `/usr/local/share/dotnet` にインストールされている必要があるみたい  

### 解決

CLI から起動すれば良い

````sh
$ export DOTNET_CLI_TELEMETRY_OPTOUT=1
$ export DOTNET_ROOT=~/.dotnet
$ <Godot install dir>/Godot_mono.app/Contents/MacOS/Godot
````

Automator を使用して Finder からダブルクリックで起動できるようにする

````sh
export DOTNET_CLI_TELEMETRY_OPTOUT=1
export DOTNET_ROOT=~/.dotnet
export GODOT_DIR=<godot install dir>

${GODOT_DIR}/Godot_mono.app/Contents/MacOS/Godot

exit 0
````

Automator は「アプリケーション」を選択し、「シェルスクリプトを実行」アクションを使用、↑のスクリプトを設定する

