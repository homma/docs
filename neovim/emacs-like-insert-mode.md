---
title: emacs like insert mode について
author: homma
status: draft
---

--------------------------------------------------------------------------------

### emacs-like insert mode について

vi の利点はモーダルであること。  
モードを切り替えて文章を編集することで、少ないキーストロークで多彩な編集を実行することができる。  

一方で、vi のデメリットもまたモーダルであること。  
モーダルであるため、モード内に実装されていない操作を実行するにはモードの切り替えが必要になる。  
例えば、挿入モードで編集した内容の保存は、一度ノーマルモードに戻って `:w` を入力することになる。  

特に挿入モード内だけで自由にテキストを編集することが難しい。  
カーソルを移動するだけでも、ホームポジションを崩して矢印キーを使うか、一旦ノーマルモードに切り替える必要がある。  
`<C-o>` で一度だけノーマルモードに戻る方法もあるが、繰り返し実行しようとすると煩雑になる。  

この問題を解決するのが `emacs-like insert mode`。  

emacs は vi のようなモーダルなエディタではない（emacs のモードは vi のモードとは異なる概念）。  
そのため、emacs を使いこなそうとすると、複数のキーを組み合わせた複雑なキーバインドを使用する必要がある。  
ただし、カーソル移動などの基本的なキー入力はとてもシンプルになっている。  
vi のノーマルモードのキー（`hjkl` など）と異なり、文字の挿入とキーが被ることもない。  

emacs のシンプルなキー入力の部分を vi の挿入モードで使用することができれば、テキスト編集がとても楽になる。  
モード切り替えが便利な場面ではモーダルの恩恵を享受することができ、最小ストロークで操作を実行できる。  
一方で、こまめにモードを切り替えたくない場面では、モードを切り替えることなく操作を実行できる。  

これを実現するには、Neovim の `init.lua` に以下を追加するだけで良い。  

````lua
-- emacs-like insert mode
vim.keymap.set('i', '<C-a>', '<Home>', { noremap = true })
vim.keymap.set('i', '<C-e>', '<End>', { noremap = true })
vim.keymap.set('i', '<C-b>', '<Left>', { noremap = true })
vim.keymap.set('i', '<C-f>', '<Right>', { noremap = true })
vim.keymap.set('i', '<C-p>', '<Up>', { noremap = true })
vim.keymap.set('i', '<C-n>', '<Down>', { noremap = true })
vim.keymap.set('i', '<C-l>', '<C-o>zz', { noremap = true })
vim.keymap.set('i', '<C-s>', '<C-o>:write<CR>', { noremap = true })
vim.keymap.set('i', '<C-q>', '<C-o>:quit<CR>', { noremap = true })
vim.keymap.set('i', '<C-d>', '<Del>', { noremap = true })
vim.keymap.set('i', '<C-k>', '<C-o>d$', { noremap = true })
````

macOS ユーザであれば、標準のテキストエディタアプリや、テキスト入力ボックスは emacs キーバインドが使用可能である。  
そのため、emacs キーバインドが vi の挿入モードでも使用できれば、大きなメリットがある。  

また、ターミナルのコマンドライン編集も emacs キーバインドで操作可能である。  
そのため、コマンドライン編集に慣れているならば、vi の挿入モードで emacs キーバインドを使用できることはメリットになる。  

同じような発想の Vim プラグインに `rsi.vim` が存在する。  

https://github.com/tpope/vim-rsi

同様の設定は GitHub 内だけでも数多く存在している。

https://github.com/search?q=vim+readline&type=repositories  
https://github.com/search?q=vim%20emacs%20insert%20mode&type=repositories

反対に、emcas をモーダルにするプログラムもある。

https://github.com/meow-edit/meow

Neovim のドキュメントにも Tips としてコマンドラインモードの Emacs-style なキーバインドについて記載がある。

https://neovim.io/doc/user/tips.html#emacs-keys  

