
# 画面の一番左にウィンドウを開く

````
:lua vim.cmd 'topleft vnew'
````

# ウィンドウを開く

````
:lua vim.cmd 'vnew'
````

# visual mode で : を入力すると、visual mode は終了してしまう
- https://vi.stackexchange.com/questions/8789/mode-always-seems-to-return-n
- https://www.reddit.com/r/neovim/comments/s2v0cz/how_to_get_the_current_mode/

: を入力した時点で visual mode が終了するため、以下は常に "n" を返す

````
:lua =vim.fn.mode()
````

visual mode で選択された文字列に対して処理を実行するためには <Cmd> を使用する必要がある

:h <cmd> に記載されている gv トリックは以下のことだと思われる

````
:lua vim.cmd 'normal! gv'
````

y でコピーしたものは以下のコードで参照できる

````
:lua =vim.fn.getreg()
````

# noremap は NOn-REcursive MAP
- https://stackoverflow.com/questions/3776117/what-is-the-difference-between-the-remap-noremap-nnoremap-and-vnoremap-mapping

map が recursive なのに対して non-recursive な map が noremap

