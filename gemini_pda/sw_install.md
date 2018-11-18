
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

#### OpenSSL Formula の修正


````sh
$ brew cat openssl
$ brew edit openssl
````

#### Roswell のインストール

````sh
$ brew install roswell
````

brew を使用せずに、以下の手順でもインストール可能であると思われる。

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
Roswell でインストール。

````sh
$ ros install cxxxr/lem
$ export PATH=${PATH}:${HOME}/.roswell/bin
$ lem
````

### Electron 動作確認

### 端末エミュレータ

### 日本語環境を整える
- mozc

## メモ

### エラー 1

openssl パッケージが arm64 に対応していない。  

`brew edit openssl` で修正する必要がある。

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