
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

### 絵文字フォントのインストール

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

### 絵文字フォントの有効化ができない

- `/etc/fonts/conf.d/70-no-bitmaps.conf` によって、ビットマップフォントが無効になっている
- このファイルを削除すると、ウィンドウバーが巨大化してしまう

対処方法は見つからず。

### コマンド

指定したディレクトリのフォントを強制的に読み込む。  
ディレクトリを指定しないと、関連する全ディレクトリを読み込むので遅い。

````sh
$ fc-cache -fv /usr/share/fonts/truetype/noto/
````

フォントが読み込まれたか確認する。

````sh
$ fc-match 'Noto Color Emoji'
````

### 絵文字フォントの有効化（失敗）

fcitx では絵文字を表示できるようになりましたが、ウィンドウバーの表示に問題が発生、パネルも異常終了してしまいました。

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
/usr/share/fonts/truetype/noto/NotoColorEmoji.ttf:
$ fc-match 'Noto Color Emoji'
NotoColorEmoji.ttf: "Noto Color Emoji" "Regular"
````

元に戻す。

````sh
$ cd /etc/fonts/conf.d
$ sudo ln -s /usr/share/fontconfig/conf.avail/70-no-bitmaps.con .
````

パネルが削除されてしまっている場合は SDDM をリスタートして強制ログアウト。

````sh
$ sudo service sddm restart
````

### 絵文字フォントの有効化（失敗 2）

これは効果がありませんでした。

70-no-bitmaps.conf
````
<?xml version="1.0"?>
<!DOCTYPE fontconfig SYSTEM "fonts.dtd">
<fontconfig>
<!-- Reject bitmap fonts -->
 <selectfont>
  <rejectfont>
   <pattern>
     <patelt name="scalable"><bool>false</bool></patelt>
   </pattern>
  </rejectfont>
  <acceptfont>
    <glob>/usr/share/fonts/truetype/noto/NotoColorEmoji.ttf</glob>
  </acceptfont>
 </selectfont>
</fontconfig>
````

````sh
$ fc-cache -vf
$ fc-match 'Noto Color Emoji'
NotoSansCJK-Medium.ttc: "Noto Sans CJK JP" "Medium"
````

元に戻す。

````sh
$ cd /etc/fonts/conf.d
$ rm 70-no-bitmaps.conf
$ sudo ln -s /usr/share/fontconfig/conf.avail/70-no-bitmaps.conf .
````
