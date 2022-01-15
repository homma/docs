````
% curl -sSL https://github.com/koka-lang/koka/releases/latest/download/install.sh | sh
Installing dependencies..
Downloading: https://github.com/koka-lang/koka/releases/download/v2.3.0/koka-v2.3.0-macos-x64.tar.gz
  % Total    % Received % Xferd  Average Speed   Time    Time     Time  Current
                                 Dload  Upload   Total   Spent    Left  Speed
100   634  100   634    0     0   2505      0 --:--:-- --:--:-- --:--:--  2496
100 17.7M  100 17.7M    0     0  11.3M      0  0:00:01  0:00:01 --:--:-- 19.0M
Unpacking..
Installing to prefix: /usr/local
- install executable            : /usr/local/bin/koka-v2.3.0
- install executable symlink    : /usr/local/bin/koka
- install pre-compiled libraries: /usr/local/lib/koka/v2.3.0
- install source libraries      : /usr/local/share/koka/v2.3.0
- install atom editor support
  Please restart Atom for Koka syntax highlighting to take effect.
Install successful.

--------------------------------------------------
Installed Koka v2.3.0 at /usr/local/bin/koka

Type 'koka' to enter the interactive compiler

daisuke@pdcd30a16 ~ % which koka
/usr/local/bin/koka
daisuke@pdcd30a16 ~ % koka
 _         _ 
| |       | |
| | _ ___ | | _ __ _
| |/ / _ \| |/ / _' |  welcome to the koka interactive compiler
|   ( (_) |   ( (_| |  version 2.3.0, Sep 20 2021, libc x64 (clang)
|_|\_\___/|_|\_\__,_|  type :? for help, and :q to quit

loading: std/core
loading: std/core/types
loading: std/core/hnd

> "あいうえお".count
check  : interactive

check  : interactive
linking: interactive
created: out/v2.3.0/clang-debug/interactive
 
5

````
