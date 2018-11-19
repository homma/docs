
## 初期設定

### Bluetooth キーボード
- Bluetooth キーボードの認識は bluetoothctl コマンドで実施
  - OS を再起動してもペアリングをキープできました

### Wi-Fi

- `iwconfig` コマンド入ってなかった..
- `connmanctl` で設定できるみたい
- `Connman UI Setup` で実行しました

### キーマップの修正
- キーボード設定から基本的なキーレイアウトは変更できました
- Caps を Ctrl にしたい

````sh
$ sudo setxkbmap -option ctrl:nocaps
$ sudo vi /etc/default/keyboard
XKBOPTIONS="ctrl:nocaps"
`````

### ターミナルの文字サイズを小さくする
- Preference から実施

### ウェブブラウザ動作確認
- chromium ブラウザ

### パッケージの設定

````sh
$ sudo apt update
$ apt search git
````

### ロケールの生成

````sh
$ sudo vi /etc/locales.gen
$ sudo dpkg-reconfigure locales
````

`/usr/sbin/locale-gen` でも良いみたい。  
`/usr/sbin` には PATH が通っていませんでした。

### ssh 接続
- sshd が動いているので、ssh 接続を確認します

- あらかじめパスワードを変更しておきます（デフォルトは gemini）
````sh
$ passwd
````

- IP アドレスを確認
````sh
$ ip addr show
````

- リモートから ssh で接続
````sh
$ ssh gemini@...
````

### motd

必要ないので消去します。

````sh
$ cd /etc
$ sudo cp motd motd.bak
$ sudo echo '' > motd
````

### マウスなしで操作できるようにする
- i3wm が良いらしい

