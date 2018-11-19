
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

Lem は Roswell でインストールします。

````sh
$ ros install cxxxr/lem
$ export PATH=${PATH}:${HOME}/.roswell/bin
$ lem
````

### Electron 動作確認

### 端末エミュレータ

### 日本語環境を整える
- mozc

