
- https://github.com/willelz/nvim-lua-guide-ja/blob/master/README.ja.md

## バッファ内の各行に対して Lua プログラムを実行する

````
luado print(line)
````

## `=` で print する
`=` で始まるコードは `print()` と同じ動きになる

````
lua ="foo"
````

## Lua のグローバル変数
`#local` を付けない値は、Lua のグローバル変数になる  
Lua のグローバル変数は、複数の `lua` コマンド間で使用できる


````
lua a = 1
lua =a
````

## Lua のグローバル変数を確認する
`command-line mode` で以下を実行する

````
=_G
````

`_G` でグローバル変数を参照できる  
`=` は `lua print()` と同じ

