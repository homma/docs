
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

負荷に応じて On/Off されるみたい。

````sh
$ lscpu 
Architecture:          aarch64
Byte Order:            Little Endian
CPU(s):                10
On-line CPU(s) list:   0-2
Off-line CPU(s) list:  3-9
Thread(s) per core:    1
Core(s) per socket:    3
Socket(s):             1
Model:                 4
Model name:            AArch64 Processor rev 4 (aarch64)
CPU max MHz:           1547.0000
CPU min MHz:           221.0000
BogoMIPS:              26.00
Flags:                 fp asimd evtstrm aes pmull sha1 sha2 crc32
$ cat /proc/cpuinfo 
            Processor: AArch64 Processor rev 4 (aarch64)
            processor: 0
            model name: AArch64 Processor rev 4 (aarch64)
            BogoMIPS: 26.00
            BogoMIPS: 26.00
            Features: fp asimd evtstrm aes pmull sha1 sha2 crc32
                CPU implementer: 0x41
CPU architecture: 8
            CPU variant: 0x0
            CPU part: 0xd03
                CPU revision: 4

            processor: 1
            model name: AArch64 Processor rev 4 (aarch64)
            BogoMIPS: 26.00
            BogoMIPS: 26.00
            Features: fp asimd evtstrm aes pmull sha1 sha2 crc32
                CPU implementer: 0x41
CPU architecture: 8
            CPU variant: 0x0
            CPU part: 0xd03
                CPU revision: 4

            processor: 2
            model name: AArch64 Processor rev 4 (aarch64)
            BogoMIPS: 26.00
            BogoMIPS: 26.00
            Features: fp asimd evtstrm aes pmull sha1 sha2 crc32
                CPU implementer: 0x41
CPU architecture: 8
            CPU variant: 0x0
            CPU part: 0xd03
                CPU revision: 4

            Hardware: MT6797X
````