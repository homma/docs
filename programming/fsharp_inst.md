## F# のインストール

### dotnet-sdk のインストール

#### インストール

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

#### 起動確認

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

### dotnet-sdk-preview のインストール

#### インストール

````
% brew tap homebrew/cask-versions
% brew cask install dotnet-sdk-preview
==> Satisfying dependencies
==> Downloading https://download.visualstudio.microsoft.com/download/pr/afcef2c8
######################################################################## 100.0%
==> Verifying SHA-256 checksum for Cask 'dotnet-sdk-preview'.
==> Installing Cask dotnet-sdk-preview
==> Running installer for dotnet-sdk-preview; your password may be necessary.
==> Package installers may write to any location; options such as --appdir are i
Password:
installer: Package name is Microsoft .NET Core SDK 3.0.100 - preview5 (x64)
installer: Installing at base path /
installer: The install was successful.
🍺  dotnet-sdk-preview was successfully installed!
````

#### 起動確認

````
% which dotnet
/usr/local/share/dotnet/dotnet
% dotnet --version
3.0.100-preview5-011568
````

## F# のアンインストール

### dotnet-sdk のアンインストール

````
% brew cask uninstall dotnet-sdk
==> Uninstalling Cask dotnet-sdk
==> Uninstalling packages:
com.microsoft.dotnet.hostfxr.2.2.4.component.osx.x64
Password:
com.microsoft.dotnet.sharedhost.component.osx.x64
com.microsoft.dotnet.sharedframework.Microsoft.NETCore.App.2.2.4.component.osx.x64
com.microsoft.dotnet.dev.2.2.106.component.osx.x64
==> Removing files:
/etc/paths.d/dotnet
/etc/paths.d/dotnet-cli-tools
==> Purging files for version 2.2.106 of Cask dotnet-sdk
````

