
# ダッシュモードの実装
- 英語では sprint と呼ぶみたい

- ダッシュキーを Ctrl に割り当てると、ダッシュしながらジャンプは Ctrl-Space になって日本語入力プログラムとバッティングしてしまう...
- wasd を esdf に変更し、ダッシュキーを a に割り当てる
- wasd のままにしたい場合は、ダッシュキーを shift に割り当てる
- shift はほとんど使わないので

````
local swift_key = "aux1"
local swift_speed = 2.0
local default = 1

local function set_swift(player)
  player:set_physics_override({speed = swift_speed})
end

local function set_no_swift(player)
  player:set_physics_override({speed = default})
end

local function is_swift_key_on(player)
  return player:get_player_control()[swift_key]
end

minetest.register_globalstep(function(dtime)
  local players = minetest.get_connected_players()
  for i = 1, #players do
    local player = players[i]
    if(is_swift_key_on(player)) then
      set_swift(player)
    else
      set_no_swift(player)
    end
  end
end)
````
