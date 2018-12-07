
## 追加ソフトウェアのインストール

### git

- デフォルトではインストールされていない
- linuxbrew のインストール時にインストールしました

### nodebrew

brew でインストール。

````sh
$ brew install nodebrew
$ nodebrew setup_dirs
````

arm64 に変更。
````sh
$ chmod +w /home/linuxbrew/.linuxbrew/Cellar/nodebrew/1.0.1/bin/nodebrew
$ vi /home/linuxbrew/.linuxbrew/Cellar/nodebrew/1.0.1/bin/nodebrew
     } elsif ($machine =~ m/aarch64/) {
-        $arch = 'armv7l';
+        $arch = 'arm64';
     } elsif ($sysname =~ m/sunos/i) {

$ chmod -w /home/linuxbrew/.linuxbrew/Cellar/nodebrew/1.0.1/bin/nodebrew
````

Issue でお知らせした方が良いかも。
- https://github.com/hokaccha/nodebrew/blob/master/nodebrew

### node.js

nodebrew でインストール。

````sh
$ nodebrew install-binary latest
$ nodebrew use latest
$ export PATH=${HOME}/.nodebrew/current/bin:${PATH}
$ node -v
v11.2.0
````

### Roswell

Lem のために必要。  
brew でインストール。

#### OpenSSL のインストール

`brew edit` で formula を修正してからインストールします。

````sh
$ brew edit openssl
# 以下の diff の内容を反映させる
@@ -39,6 +39,7 @@ class Openssl < Formula
       :i386 => %w[linux-generic32],
       :x86_64 => %w[linux-x86_64],
       :arm => %w[linux-armv4],
+      :arm64 => %w[linux-aarch64],
     } if OS.linux?
 
     {
@@ -75,6 +76,8 @@ class Openssl < Formula
       arch = Hardware::CPU.arch_32_bit
     end
 
+    arch = :arm64
+
     ENV.deparallelize
     system "perl", "./Configure", *(configure_args + arch_args[arch])
     system "make", "depend"

$ brew install openssl
````

#### Roswell のインストール

````sh
$ brew install roswell
````

brew を使用せずに、以下の手順でもインストール可能であると思われます。

````sh
$ git clone -b release https://github.com/roswell/roswell.git
$ cd roswell
$ sh bootstrap
$ ./configure --prefix=${HOME}/.local
$ make
$ sudo make install
````

- https://github.com/homma/docs/blob/master/lem/android.md

### Lem

事前に ncurses のインストールが必要です。  

````sh
$ brew install ncurses
````

`C_INCLUDE_PATH` を追加していない場合は設定します。  
Linuxbrew で GCC をインストールした場合は、この設定は必要ないかもしれません。

````sh
$ export C_INCLUDE_PATH=/home/linuxbrew/.linuxbrew/include
````

Lem は Roswell でインストールします。

````sh
$ ros install cxxxr/lem
$ export PATH=${PATH}:${HOME}/.roswell/bin
$ lem
````

起動時間は 1 秒程度でした。

### GCC インストール

Linuxbrew でインストールするソフトウェアのコンパイル用。  
apt でインストールした GCC を使用すると、`C_INCLUDE_PATH` の設定が必要になりそうなので、トラブル避けのために専用の GCC をインストールしておきます。

````sh
$ brew install gcc
````

GCC のインストールには 3 時間以上かかりました。

````sh
==> Installing gcc
==> Downloading https://ftp.gnu.org/gnu/gcc/gcc-5.5.0/gcc-5.5.0.tar.xz
######################################################################## 100.0%
==> ../configure --with-isl=/home/linuxbrew/.linuxbrew/opt/isl@0.18 --with-bugur
==> make
==> make install-strip
==> Creating the GCC specs file: /home/linuxbrew/.linuxbrew/Cellar/gcc/5.5.0_4/l
🍺  /home/linuxbrew/.linuxbrew/Cellar/gcc/5.5.0_4: 1,252 files, 123.3MB, built in 193 minutes 16 seconds
````

途中で生成されるファイルなどでディスクスペースも数 GB 必要なようです。  
最終的なディスク使用量は 0.3GB 程度のようでした。

include search パスは正しく設定されています。

````sh
$ gcc -xc -E -v -
...
#include <...> search starts here:
 /home/linuxbrew/.linuxbrew/include
 /home/linuxbrew/.linuxbrew/Cellar/gcc/5.5.0_4/lib/gcc/aarch64-unknown-linux-gnu/5.5.0/include
 /home/linuxbrew/.linuxbrew/Cellar/gcc/5.5.0_4/include
 /home/linuxbrew/.linuxbrew/Cellar/gcc/5.5.0_4/lib/gcc/aarch64-unknown-linux-gnu/5.5.0/include-fixed
 /usr/include/aarch64-linux-gnu
 /usr/include
````

### 日本語環境を整える

- ./ime.md 参照

### 端末エミュレータ

qterminal で日本語入力も問題ありませんでした。

- ./terminal.md 参照

### w3m

Linuxbrew でインストールします。

`configure` が失敗するため、`superenv.rb` を修正します。

````sh
$ sudo vi /home/linuxbrew/.linuxbrew/Homebrew/Library/Homebrew/extend/ENV/super.rb
// "-march=native" を削除する
````

変更内容は以下の通り。

````diff
$ git diff
diff --git a/Library/Homebrew/extend/ENV/super.rb b/Library/Homebrew/extend/ENV/super.rb
index 2b5951fef..a2e03093a 100644
--- a/Library/Homebrew/extend/ENV/super.rb
+++ b/Library/Homebrew/extend/ENV/super.rb
@@ -236,7 +236,7 @@ module Superenv
     elsif Hardware::CPU.intel? && !Hardware::CPU.sse4?
       Hardware::CPU.optimization_flags.fetch(Hardware.oldest_cpu)
     elsif ![:gcc_4_0, :gcc_4_2].include?(compiler)
-      "-march=native"
+      ""
     # This is mutated elsewhere, so return an empty string in this case
     else
       ""
````

修正が完了したらインストールを行います。

````sh
$ brew install w3m
==> Installing dependencies for w3m: libatomic_ops, bdw-gc, libbsd and gettext
...
````

動作確認。

````sh
$ w3m -O utf8 www.google.com
````

### Common Lisp HyperSpec

Linuxbrew でインストールします。

````sh
$ brew install hyperspec
````

### tmux

ターミナルをフルスクリーンにした状態で使うことが多いと思われるため tmux をインストールします。  
Linuxbrew でインストール。

````sh
$ brew install tmux
==> Installing dependencies for tmux: gdbm, readline, sqlite, bzip2, python@2, sphinx-doc, cmake, bison, help2man, flex, doxygen, libtool and libevent
...
````

ひとまず `--with-utf8proc` はなしでインストールしました。

### フォント

- [移動](./fonts.md)

### xdotool

Caps Lock が有効になってしまい、解除する方法がなかったので、xdotool をインストールしました。

````sh
$ sudo apt install xdotool
````

xdotool と libxdo3 がインストールされました。  

````sh
$ xdotool key Caps_Lock
````

Linuxbrew からインストールすると Xorg のビルドが必要なのでやめました。

### Electron 動作確認
