## F# のインストール

### dotnet-sdk のインストール

brew cask に dotnet-sdk があります。

````
% brew search dotnet-sdk
==> Casks
dotnet-sdk
homebrew/cask-versions/dotnet-sdk-preview
````

これをインストールします。

````
% brew cask install dotnet-sdk
==> Satisfying dependencies
==> Downloading https://download.visualstudio.microsoft.com/download/pr/4d0f3f47
######################################################################## 100.0%
==> Verifying SHA-256 checksum for Cask 'dotnet-sdk'.
==> Installing Cask dotnet-sdk
==> Running installer for dotnet-sdk; your password may be necessary.
==> Package installers may write to any location; options such as --appdir are i
Password:
installer: Package name is Microsoft .NET Core SDK 2.2.106 (x64)
installer: Installing at base path /
installer: The install was successful.
🍺  dotnet-sdk was successfully installed!
````

起動確認

````
% which dotnet
/usr/local/share/dotnet/dotnet
% dotnet --version
2.2.106
````

dotnet コマンドに PATH が通っていない場合は、シェルの設定ファイル（.zshrc など）に以下を追加します。

````
eval $(/usr/libexec/path_helper -s)
````

## インタラクティブシェル (FSI) の実行

### エイリアスの作成

````sh
% alias fsi="dotnet $(dirname $(which dotnet))/sdk/$(dotnet --version)/FSharp/fsi.exe --nologo"
````

### 実行

````
% fsi

> printf "hello,world\n";;
hello,world
val it : unit = ()

> #q;;
````

