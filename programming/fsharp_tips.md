## FSI

### FSI から C のライブラリを使用できない

以下のエラーが発生する。

````
System.Security.SecurityException: ECall methods must be packaged into a system module.
````

- https://github.com/fsharp/fsharp/issues/886
- https://github.com/dotnet/corefx/pull/31096

ワークアラウンドはないっぽい。

### 履歴編集はできないっぽい

`Ctrl-P` や `Ctrl-N` は効かない。

### Delete が正しく受け渡されていないみたい

以下の例では、最初の `x` の前に間違えて `=` を入力してしまってから削除しています。

````
> let double x = x * x;;

  let double =x = x * x;;
  ------------^
````

### FSI の終了は `Ctrl-C`

`Ctrl-D` ではシェルを抜けられない。

`Ctrl-D` + `Return` や `Ctrl-D` + `Ctrl-D` では抜けられる。

### FSI 実行時のヘルプは `--help`

````
% fsi --help

使い方: dotnet <オプション> [script.fsx [<引数>]]

...
````

### インタラクティブシェルのヘルプは `#help;;`

````
> #help;;

  F# Interactive ディレクティブ:

    #r "file.dll";;        指定された DLL を参照します (動的読み込み)
    #I "path";;            参照されている DLL に対し、指定された検索パスを追加します
    #load "file.fs" ...;;  コンパイルおよび参照されているように、指定されたファイルを読み込みます
    #time ["on"|"off"];;   タイミングのオンとオフを切り替えます
    #help;;                ヘルプの表示
    #quit;;                終了
````

