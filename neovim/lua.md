
- https://github.com/willelz/nvim-lua-guide-ja/blob/master/README.ja.md

バッファ内の各行に対して Lua プログラムを実行する

````
luado print(line)
````

`=` で始まるコードは `print()` と同じ動きになる

````
lua ="foo"
````

`local` を付けない値は、複数の `lua` コマンド間で使用できる

````
lua a = 1
lua =a
````
