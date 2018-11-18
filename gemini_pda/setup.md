
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

### マウスなしで操作できるようにする
- i3wm が良いらしい

## 基本情報

````
$ cat /etc/os-release  
PRETTY_NAME="Debian GNU/Linux 9 (stretch)"
NAME="Debian GNU/Linux"
VERSION_ID="9"
VERSION="9 (stretch)"
ID=debian
HOME_URL="https://www.debian.org/"
SUPPORT_URL="https://www.debian.org/support"
BUG_REPORT_URL="https://bugs.debian.org/"
$ uname -a
Linux localhost 3.18.41+ #7 SMP PREEMPT Wed May 30 16:45:45 MSK 2018 aarch64 GNU/Linux
$ df -h
Filesystem       Size  Used Avail Use% Mounted on
udev             1.9G     0  1.9G   0% /dev
tmpfs            383M   40M  344M  11% /run
/dev/mmcblk0p29   57G  3.2G   51G   6% /
tmpfs            1.9G   37M  1.9G   2% /dev/shm
tmpfs            5.0M     0  5.0M   0% /run/lock
tmpfs            1.9G     0  1.9G   0% /sys/fs/cgroup
/dev/loop0       649M  621M   14M  98% /system
none             1.9G  4.5M  1.9G   1% /var/lib/lxc/android/rootfs
/dev/mmcblk0p28  419M  396K  410M   1% /cache
/dev/mmcblk0p8   3.9M   68K  3.7M   2% /protect_f
/dev/mmcblk0p9   8.3M   60K  8.0M   1% /protect_s
/dev/mmcblk0p5   3.9M   44K  3.7M   2% /nvcfg
/dev/mmcblk0p6    28M  7.7M   20M  29% /nvdata
tmpfs            383M  4.0K  383M   1% /run/user/100000
$ lscpu 
Architecture:          aarch64
Byte Order:            Little Endian
CPU(s):                10
On-line CPU(s) list:   0
Off-line CPU(s) list:  1-9
Thread(s) per core:    1
Core(s) per socket:    1
Socket(s):             1
Model:                 4
Model name:            AArch64 Processor rev 4 (aarch64)
CPU max MHz:           1547.0000
CPU min MHz:           221.0000
BogoMIPS:              26.00
Flags:                 fp asimd evtstrm aes pmull sha1 sha2 crc32
$ head /proc/meminfo 
MemTotal:        3914596 kB
MemFree:         2185624 kB
MemAvailable:    2871724 kB
Buffers:           41880 kB
Cached:           734048 kB
SwapCached:            0 kB
Active:           516804 kB
Inactive:         626688 kB
Active(anon):     372900 kB
Inactive(anon):    95872 kB
$ pwd
/home/gemini
$ id
uid=100000(gemini) gid=100000(gemini) groups=100000(gemini),27(sudo),29(audio),44(video),1000(aid_system),1003(aid_graphics),1004(aid_input),1005(aid_audio),3001(aid_net_bt_admin),3002(aid_net_bt),3003(aid_inet),3004(aid_inet_raw),3005(aid_inet_admin)
$ ls -1
./
../
.Xauthority
.bash_history
.bash_logout
.bashrc
.cache/
.config/
.gnupg/
.gtkrc-2.0
.lesshst
.local/
.pki/
.profile
.wget-hsts
.xsession-errors
Desktop/
Documents/
Downloads/
Music/
Pictures/
Public/
Templates/
Videos/
````