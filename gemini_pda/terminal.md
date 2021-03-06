
### デフォルトで使用する Terminal の変更

`update-alternatives` コマンドで変更します。

````sh
$ sudo update-alternatives --config x-terminal-emulator
There are 2 choices for the alternative x-terminal-emulator (providing /usr/bin/x-terminal-emulator).

  Selection    Path                Priority   Status
------------------------------------------------------------
* 0            /usr/bin/qterminal   40        auto mode
  1            /usr/bin/mlterm      20        manual mode
  2            /usr/bin/qterminal   40        manual mode

Press <enter> to keep the current choice[*], or type selection number: 
````

### rxvt

#### インストール

Linuxbrew でインストールできませんでした。

Linuxbrew に 256 color 版の rxvt-unicode があったので、これをインストールしてみました。

````
$ brew install rxvt-unicode
````

configure のエラーが発生してビルド失敗。

aarch64 では、GCC 8 になるまで -march=native が使用できない。

https://www.phoronix.com/scan.php?page=news_item&px=GCC-8-march-native-ARM

`superenv.rb` を編集することで解決済みです。

### mlterm

qterminal は動作が遅いため、mlterm を使ってみます。

#### インストール

インストールはできました。  
動作も概ね問題なし。

削除する際は、mlterm, mlterm-common, mlterm-tools, mlterm-im-fcitx を削除

````sh
$ sudo apt install mlterm
Reading package lists... Done
Building dependency tree       
Reading state information... Done
The following additional packages will be installed:
  mlterm-common mlterm-tools
Suggested packages:
  unifont xfonts-efont-unicode fonts-vlgothic | fonts-japanese-gothic
  fonts-nanum | fonts-baekmuk fonts-arphic-bsmi00lp fonts-arphic-gbsn00lp
  fonts-freefont-ttf t1-cyrillic mlterm-im-uim mlterm-im-m17nlib
  mlterm-im-scim
The following NEW packages will be installed:
  mlterm mlterm-common mlterm-tools
0 upgraded, 3 newly installed, 0 to remove and 151 not upgraded.
Need to get 1290 kB of archives.
After this operation, 10.5 MB of additional disk space will be used.
Do you want to continue? [Y/n] y
...
$ sudo apt install mlterm-im-fcitx
Reading package lists... Done
Building dependency tree       
Reading state information... Done
The following NEW packages will be installed:
  mlterm-im-fcitx
0 upgraded, 1 newly installed, 0 to remove and 151 not upgraded.
Need to get 4576 B of archives.
After this operation, 20.5 kB of additional disk space will be used.
...
````

#### フルスクリーンにしたい

設定が見つからず。

#### 設定

大元の設定ファイルは `/etc/mlterm/` 以下にあります。  
これを `~/.mlterm/` 以下にコピーして修正します。

#### フォントサイズの変更

フォントのサイズが小さいため大きくします。  
フォントサイズは 20 がちょうど良いみたい。

#### フォントの変更

デフォルトで使用されているフォントは英字の幅が狭いため変更します。

`xlsfonts` や `fc-list` でフォントを確認します。

#### スクロールバーの位置の変更

デフォルトではスクロールバーが左にあるため、右に変更します。

#### メタキーの挙動の変更

デフォルトでは Alt キーが効かないため、設定を変更します。