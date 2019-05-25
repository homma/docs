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

