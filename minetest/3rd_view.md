
Third Person View のカメラ位置の調整
常に Third Person View にカメラを固定 -- これは実現できず

# Global Mod

````
minetest.register_globalstep(function(dtime)
  local players = minetest.get_connected_players()
  for i = 1, #players do
    players[i]:set_eye_offset({x=0, y=0, z=0}, {x=0, y=15, z=-10})
  end
end)
````

# Client Mod

- https://github.com/minetest/minetest/issues/10142

- 以下のコードはクラッシュする
````
minetest.register_globalstep(function(dtime)
  if minetest.localplayer then
    minetest.camera:set_camera_mode(1)
  end
end)
````

# 問題点

## カメラがキャラクターに寄った時にポインタがずれる
- プレイヤーの位置と周囲のオブジェクトによっては、強制的にカメラがキャラに寄ります
- その時に Crosshair が指しているオブジェクトと、操作対象のオブジェクトにずれが生じることがあります

## Wield Item について
- デフォルトでは手に持ったアイテムが表示されない
- WieldView Mod などを使用することで表示できる
- https://forum.minetest.net/viewtopic.php?t=25358
