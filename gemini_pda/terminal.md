### rxvt

Linuxbrew でインストールできませんでした。

Linuxbrew に 256 color 版の rxvt-unicode があったので、これをインストールしてみました。

````
$ brew install rxvt-unicode
````

configure のエラーが発生してビルド失敗。

aarch64 では、GCC 8 になるまで -march=native が使用できない。

https://www.phoronix.com/scan.php?page=news_item&px=GCC-8-march-native-ARM

### mlterm

インストールはできました。  
動作も概ね問題なし。

qterminal を使わない積極的な理由は今の所なし。

mlterm, mlterm-common, mlterm-tools, mlterm-im-fcitx は削除しても良いかも。

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

