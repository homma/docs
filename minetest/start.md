
# Minetest のインストール

````
$ brew install minetest
$ alias mtest=/usr/local/opt/minetest/minetest.app/Contents/MacOS/minetest
````

# インストールされる場所

- /usr/local/opt/minetest/

# 設定ディレクトリの場所

- `~/Library/Application Support/minetest`

# Minetest の設定ディレクトリにアクセスしやすくする

````
$ ln -s Library/Application\ Support/minetest/ ~/.minetest
````

# マップ生成

- デフォルトでは v7 が選択されているが、v7 は起伏が激しく平地がないため、v6 を選択してマップを生成する
- v6 は v7 より古いから使えないという訳ではなく、別タイプのマップ生成機能として並行で開発されている

# 生成されたマップファイルの場所

- `~/Library/Application Support/minetest/worlds/`

# 飛行モードを有効化

- クリエイティブモードを使用
- `/` -> `/grant singleplayer all`
- `k` で飛行モードを有効化
- `Space` で上昇
- `Shift` で下降

