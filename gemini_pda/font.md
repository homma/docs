
## フォントの設定

### フォントの確認

Noto が設定されている。

````sh
$ fc-match sans
NotoSansCJK-Medium.ttc: "Noto Sans CJK JP" "Medium"
$ fc-match serif
NotoSansCJK-Medium.ttc: "Noto Sans CJK JP" "Medium"
$ fc-match monospace
NotoSansCJK-Regular.ttc: "Noto Sans Mono CJK JP" "Regular"
````

### Noto Color Emoji

#### 絵文字フォントのインストール

絵文字のフォントが必要。

- Linuxbrew cask : font-noto-color-emoji
- apt : fonts-noto-color-emoji

他の Noto フォントは apt でインストールされていたので、`fonts-noto-color-emoji` も apt でインストールしました。

````sh
$ sudo apt install fonts-noto-color-emoji
````

以下のパスにインストールされます。

````
/usr/share/fonts/truetype/noto/NotoColorEmoji.ttf                                
````

#### 絵文字フォントの有効化ができない

- `/etc/fonts/conf.d/70-no-bitmaps.conf` によって、ビットマップフォントが無効になっている
- このファイルを削除すると、ウィンドウバーが巨大化してしまう

対処方法は見つからず。

#### コマンド

指定したディレクトリのフォントを強制的に読み込む。  
ディレクトリを指定しないと、関連する全ディレクトリを読み込むので遅い。

````sh
$ fc-cache -fv /usr/share/fonts/truetype/noto/
````

フォントが読み込まれたか確認する。

````sh
$ fc-match 'Noto Color Emoji'
````

#### 絵文字フォントの有効化（失敗）

- fcitx では絵文字を表示できる
- ウィンドウバーの表示に問題が発生（バーが太くなる）
- パネルが異常終了する

- バーが太くなるのは `Noto Color Emoji` に合わせているっぽい
- パネルが異常終了するのも、フォントサイズが大きいからかも

ビットマップフォントが拒否されているので、設定ファイルを削除する。

````sh
$ cd /etc/fonts/conf.d
$ ls -l 70-no-bitmaps.conf
lrwxrwxrwx 1 root root 51  6月  1 05:40 70-no-bitmaps.conf -> /usr/share/fontconfig/conf.avail/70-no-bitmaps.con
$ sudo rm 70-no-bitmaps.conf
````

確認。

````sh
$ fc-list : file | grep -i emoji
/usr/share/fonts/truetype/noto/notocoloremoji.ttf:
$ fc-match 'noto color emoji'
notocoloremoji.ttf: "noto color emoji" "regular"
````

元に戻す。

````sh
$ cd /etc/fonts/conf.d
$ sudo ln -s /usr/share/fontconfig/conf.avail/70-no-bitmaps.conf .
````

パネルが削除されてしまっている場合は sddm をリスタートして強制ログアウト。

````sh
$ sudo service sddm restart
````

#### 絵文字フォントの有効化（失敗 2）

多少改善したものの、常用は不可能。

- fcitx では使用可能（ただしモノクロ表示）
- chromium の表示画面では使用できない（tofu になる）
- ウィンドウバーに絵文字が表示されると、表示が大きくなる
- ウィンドウバーに絵文字が表示されるとパネルに異常が発生する
- qterminal に絵文字が表示されると qterminal が落ちる
- mlterm では tofu になる

chromium、パネル、qterminal、mlterm はフォント指定の問題であると思われる。

設定:
````
$ mkdir -p ~/.config/fontconfig/conf.d
$ cd -
$ vi 10-noto-color-emoji.conf 
<?xml version="1.0"?>
<!DOCTYPE fontconfig SYSTEM "fonts.dtd">
<fontconfig>
 <selectfont>
  <acceptfont>
   <pattern>
     <patelt name="family"><string>Noto Color Emoji</string></patelt>
   </pattern>
  </acceptfont>
 </selectfont>
</fontconfig>

$ fc-match 'Noto Color Emoji'
NotoColorEmoji.ttf: "Noto Color Emoji" "Regular"
````

### TwitterColorEmoji-SVGinOT

モノクロ表示ですが、問題なく動作しました。  
モノクロのフォントはアウトラインフォントなので、ビットマップフォントの問題は発生しない。

とりあえずはこれで OK とします。

#### フォントのインストール

````sh
$ mkdir -p ~/Applications/twemoji-color-font
$ cd $_
$ curl -L -# -O https://github.com/eosrei/twemoji-color-font/releases/download/v11.2.0/TwitterColorEmoji-SVGinOT-Linux-11.2.0.tar.gz
$ tar xf TwitterColorEmoji-SVGinOT-Linux-11.2.0.tar.gz 
$ cd TwitterColorEmoji-SVGinOT-Linux-11.2.0/
$ mkdir ~/.local/share/fonts
$ cp TwitterColorEmoji-SVGinOT.ttf ~/.local/share/fonts/
$ cp fontconfig/56-twemoji-color.conf ~/.config/fontconfig/conf.d/
$ fc-cache -f ~/.local/share/fonts/
$ fc-match 'Twitter Color Emoji'
TwitterColorEmoji-SVGinOT.ttf: "Twitter Color Emoji" "Regular"
````

#### 動作確認

- Chromium : OK
- qterminal : 表示できない文字がある
- mlterm : 表示できない

vera フォントをインストールすれば良いのかな。
