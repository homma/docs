## 日本語環境を整える

### この手順について

以下の手順を参考にさせていただきました。

- https://matoken.org/blog/2018/10/23/debian-ubuntu-borrows-the-upstream-package/

### mozc のパッケージがない

Linuxbrew にはないので、以下のいずれかの手順が必要です。

- Linuxbrew の formula を作成する
- apt でインストールする
- 自分でビルドする

mozc はビルドツールが独特だったので、自前のビルドは大変かも。

と思いましたが、そもそも `mozc` のパッケージがない。

````
$ apt search ibus-mozc
Sorting... Done
Full Text Search... Done
$ apt search fcitx-mozc
Sorting... Done
Full Text Search... Done
````

ここにないからないのかな。

- https://packages.debian.org/ja/stretch/ibus-mozc
- https://packages.debian.org/ja/stretch/utils/fcitx-mozc

プログラミング用の端末なので日本語の必要性はそれほど高くはないですが。

こちらを参考に、`buster` から取得することにします。
- https://matoken.org/blog/2018/10/23/debian-ubuntu-borrows-the-upstream-package/

### sources.list がない

`buster` からパッケージを取得するために `sources.list` の編集が必要ですが、`/etc/apt/sources.list` が存在していませんでした。

````sh
$ ls /etc/apt/sources.list
ls: cannot access '/etc/apt/sources.list': No such file or directory
$ ls /etc/apt/            
./   apt.conf.d/     sources.list.d/  trusted.gpg.d/
../  preferences.d/  trusted.gpg
````

`/usr/share` にはある。
````sh
$ ls /usr/share/doc/apt/examples/sources.list 
/usr/share/doc/apt/examples/sources.list
$ cat /usr/share/doc/apt/examples/sources.list
...
deb http://deb.debian.org/debian stretch main contrib non-free
deb http://security.debian.org stretch/updates main contrib non-free
...
````

apt コマンドは gemian.thinkglobally.org を見に行っている。

````sh
$ sudo apt update
Get:1 http://gemian.thinkglobally.org/stretch stretch InRelease [3925 B]       
Get:3 http://gemian.thinkglobally.org/stretch stretch/main Sources [17.4 kB]   
Ign:2 http://cdn-fastly.deb.debian.org/debian stretch InRelease
Get:5 http://gemian.thinkglobally.org/stretch stretch/main arm64 Packages [87.4 kB]
Hit:4 http://cdn-fastly.deb.debian.org/debian stretch Release
Fetched 109 kB in 2s (37.4 kB/s)                             
Reading package lists... Done
Building dependency tree       
Reading state information... Done
151 packages can be upgraded. Run 'apt list --upgradable' to see them.
````

`/etc/apt/sources.list.d` から来ている模様。

````sh
$ ls /etc/apt/sources.list.d/
./  ../  multistrap-debian.list  multistrap-gemian.list
$ cat multistrap-debian.list 
deb [arch=arm64] http://http.debian.net/debian stretch main contrib non-free
deb-src http://http.debian.net/debian stretch main contrib non-free
$ cat multistrap-gemian.list 
deb [arch=arm64] http://gemian.thinkglobally.org/stretch/ stretch main
deb-src http://gemian.thinkglobally.org/stretch/ stretch main
````

### preferences がない

これはなくても良いみたい。

````sh
$ ls /etc/apt/preferences   
ls: cannot access '/etc/apt/preferences': No such file or directory
$ ls /etc/apt/preferences.d/
./  ../
````

````sh
$ cat /usr/share/doc/apt/examples/preferences 
Package:  *
Pin:  release a=stable
Pin-Priority:  500

Package:  *
Pin:  release a=testing
Pin-Priority:  101

Package:  *
Pin:  release a=unstable
Pin-Priority:  99
````

### multistrap-debian.list を編集

````
$ sudo apt edit-sources multistrap-debian.list
deb http://deb.debian.org/debian buster main

$ cat /etc/apt/sources.list.d/multistrap-debian.list 
deb [arch=arm64] http://http.debian.net/debian stretch main contrib non-free
deb-src http://http.debian.net/debian stretch main contrib non-free
deb http://deb.debian.org/debian buster main
````

### preference を追加

````sh
$ sudo vi /etc/apt/preferences
Package: *
Pin: release n=buster
Pin-Priority: 100
````

### パッケージ情報の更新

````sh
$ sudo apt update
````

### mozc パッケージの検索

````sh
$ apt search mozc
Sorting... Done
Full Text Search... Done
emacs-mozc/testing 2.23.2815.102+dfsg-2+b1 arm64
  Mozc for Emacs

emacs-mozc-bin/testing 2.23.2815.102+dfsg-2+b1 arm64
  Helper module for emacs-mozc

fcitx-dbus-status/testing 2016062301-2 arm64
  Addon for Fcitx to set/get/monitor IM statuses via D-Bus

fcitx-mozc/testing 2.23.2815.102+dfsg-2+b1 arm64
  Mozc engine for fcitx - Client of the Mozc input method

ibus-mozc/testing 2.23.2815.102+dfsg-2+b1 arm64
  Mozc engine for IBus - Client of the Mozc input method

mozc-data/stable 2.19.2623.102+dfsg-1 all
  Mozc input method - data files

mozc-server/testing 2.23.2815.102+dfsg-2+b1 arm64
  Server of the Mozc input method

mozc-utils-gui/testing 2.23.2815.102+dfsg-2+b1 arm64
  GUI utilities of the Mozc input method

uim-mozc/testing 2.23.2815.102+dfsg-2+b1 arm64
  Mozc engine for uim - Client of the Mozc input method

````
