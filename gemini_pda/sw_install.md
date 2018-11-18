
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

````sh
$ brew install roswell
````

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
