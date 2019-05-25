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

## コンパイル

### スタンドアローンの実行ファイルを作成する

以下でできるみたい（要確認）。

````
% dotnet publish -c Release -r osx-x64
````

スタンドアローンといっても、ランタイムを同梱している形式みたい。

#### リファレンス
- https://docs.microsoft.com/ja-jp/dotnet/core/tools/dotnet-publish?tabs=netcore21
- https://docs.microsoft.com/ja-jp/dotnet/core/rid-catalog

