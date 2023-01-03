# ミニマップを有効化

## マッピングキットを作成する

- マッピングキットを作成するとミニマップを表示できるようになるみたい
- https://wiki.minetest.net/Mapping_Kit

## クリエイティブモードを使用する

- クリエイティブモードでは `v` キーでミニマップを表示できる

## 以下は現在は実行できない模様

- デフォルトではミニマップが表示されないようになっています
- 設定で有効化することはできないため、Mod を作成します
- Client Mod は明示的にワールドの Mod として選択する必要はなさそう
  - 洗濯ができず、自動で読み込まれる

````
% mkdir -p ~/.minetest/clientmods/minimap
% vi ~/.minetest/clientmods/minimap/init.lua
% cat ~/.minetest/clientmods/minimap/init.lua
minetest.ui.minimap:show()
% vi ~/.minetest/clientmods/minimap/mod.conf 
% cat ~/.minetest/clientmods/minimap/mod.conf
name = minimap
description = always enable minimap
% vi ~/.minetest/minetest.conf
enable_client_modding = true
% vi ~/.minetest/clientmods/mods.conf
load_mod_minimap = true
````

- おそらく以下は不要
- 設定 >> すべての設定 >> クライアント >> ネットワーク >> クライアントの改造 >> 編集 >> 有効 >> 保存

- クライアント Mod を使用する場合は、クライアント Mod を有効化する必要がある
- https://github.com/minetest/minetest/blob/master/doc/client_lua_api.txt
- https://forum.minetest.net/viewtopic.php?t=17830
