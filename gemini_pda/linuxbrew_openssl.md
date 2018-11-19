
## 64bit ARM Linux の Linuxbrew で OpenSSL をビルドする

### 事前情報

#### Linuxbrew のプラットフォーム名

64bit ARM の `arch_64_bit 名` は `arm64`

https://github.com/Linuxbrew/brew/blob/master/Library/Homebrew/hardware.rb
````ruby
      def arch_64_bit
        if arm?
          :arm64
````

#### OpenSSL のプラットフォーム名

64bit ARM のプラットフォーム名は `linux-aarch64`

````sh
$ brew cat openssl > openssl.rb
$ $ grep url openssl.rb 
  url "https://www.openssl.org/source/openssl-1.0.2p.tar.gz"
...
$ curl -# -O https://www.openssl.org/source/openssl-1.0.2p.tar.gz
$ tar xf openssl-1.0.2p.tar.gz
$ cd openssl-1.0.2p
$ ./Configure LIST
...
linux-aarch64
...
````

参考 : https://wiki.openssl.org/index.php/Compilation_and_Installation

### formula の編集

`brew edit` で OpenSSL 用の `formula` を書き換えます。

````sh
$ brew edit openssl
````

書き換える内容は、以下の通り `arm64` 用のエントリーを追加します。  
ad-hoc な対応ですが、とりあえずは OK。

````diff
diff --git a/openssl.rb b/openssl.rb.mod
index 2218f29..5c1c4a0 100644
--- a/openssl.rb
+++ b/openssl.rb.mod
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
````

### OpenSSL のインストール

````
$ brew install openssl
````

## エラーの記録

### formula を修正せずにインストールしようとした場合のエラー。

````
==> Installing roswell dependency: openssl
==> Downloading https://www.openssl.org/source/openssl-1.0.2p.tar.gz
/usr/bin/curl -q --show-error --user-agent Linuxbrew/1.8.2\ \(Linux\;\ aarch64\ 3.18.41\+\)\ curl/7.52.1 --fail --location --remote-time --continue-at 0 --output /home/gemini/.cache/Homebrew/downloads/6d2f0aa30538560efe2aae756229a9ced40e636a70083696fb1bceb6c1a7564c--openssl-1.0.2p.tar.gz.incomplete https://www.openssl.org/source/openssl-1.0.2p.tar.gz --connect-timeout 5
  % Total    % Received % Xferd  Average Speed   Time    Time     Time  Current
                                 Dload  Upload   Total   Spent    Left  Speed
100 5213k  100 5213k    0     0  1104k      0  0:00:04  0:00:04 --:--:-- 1104k
==> Verifying 6d2f0aa30538560efe2aae756229a9ced40e636a70083696fb1bceb6c1a7564c--openssl-1.0.2p.tar.gz checksum
tar xf /home/gemini/.cache/Homebrew/downloads/6d2f0aa30538560efe2aae756229a9ced40e636a70083696fb1bceb6c1a7564c--openssl-1.0.2p.tar.gz -C /tmp/d20181118-27402-19htl2a
cp -pR /tmp/d20181118-27402-19htl2a/openssl-1.0.2p/. /tmp/openssl-20181118-27402-tre0lx/openssl-1.0.2p
chmod -Rf +w /tmp/d20181118-27402-19htl2a
Error: An exception occurred within a child process:
  TypeError: no implicit conversion of nil into Array
````