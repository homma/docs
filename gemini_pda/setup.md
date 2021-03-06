
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
- `Ctrl+-` でも設定可能

### ターミナルをフルスクリーンにする

- `Fn + Shift + F11`

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

### 画面解像度の設定
- `xorg.conf` や `~/.xprofile` では設定変更できない
- `/etc/sddm.conf` を変更する必要がある

- `-dpi` は 145 がちょうど良かった
  - 144 だとウィジェットのサイズが小さすぎる（144 から急に小さくなる）
  - デフォルトの 192 だと大きすぎる（フォントのサイズが大きいため）

/etc/sddm.conf
````
# Arguments passed to the X server invocation
#ServerArguments=-nolisten tcp -dpi 192
ServerArguments=-nolisten tcp -dpi 145
````

- 画面に収まりきらないウィンドウがあった場合は一時的に `xrandr` コマンドで解像度を変更可能

````sh
$ xrandr --dpi 130
// 操作を行う
$ xrandr --dpi 145
````

- 現在の設定は `xdpyinfo` で確認可能

````sh
$ xdpyinfo | grep -B2 resolution
````

### マウスなしで操作できるようにする

- i3wm が良いらしい
- qterminal をフルスクリーンにして、tmux を使う方法で問題なさそう

- dmenu 的なものが欲しい
  - lxqt-runner というのがあるみたい

#### lxqt-runner
- `Alt-F2` で起動
  - 実際には、`Alt + Fn + Shift + F2` になり押しにくい

- キーバインドは変更可能
  - `Preferences >> LXQt Settings >> Shortcut Keys`

- `Super + r` とかが良いかもしれない

### PDF ビューワの動作確認

- qpdfview がインストールされている
- https://launchpad.net/qpdfview

- 特に問題なし
- F11 でフルスクリーン表示

### Dropbox アクセス確認
