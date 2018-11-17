
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

### マウスなしで操作できるようにする
- i3wm が良いらしい

