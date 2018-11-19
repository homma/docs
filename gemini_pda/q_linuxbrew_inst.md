Title: 64bit ARM Linux 環境に Linuxbrew をインストールする

## はじめに

Linuxbrew は root 権限なしにパッケージ管理ができる便利なツールです。
問題が発生したら丸ごと消去してしまってもシステムの動作に影響しないため、気軽に使用することができます。

2018 年 11 月現在、Linuxbrew は 64bit ARM Linux をサポートしていないため、そのままではインストールすることができません。
実際に 64bit ARM Linux 環境 (Debian 9) に Linuxbrew をインストールした手順をまとめてみました。

## 事前要件

以下の手順は、インストールするユーザに `sudo` コマンドを使用する権限があることを前提としています。
`sudo` コマンドを使用することができないユーザの場合は、インストール先のディレクトリを変更するなどの対応が必要になります。

## インストール手順

### Linuxbrew に必要なソフトウェアのインストール

以下のソフトウェアは Linuxbrew に必要なため、事前にインストールします。

````sh
$ sudo apt-get install build-essential curl file git
````

これは公式の手順と同じです。

### Ruby のインストール

Linuxbrew は独自の Ruby バイナリをインストールして使用しています。
この Ruby バイナリは 64bit ARM Linux のものが用意されていないため、自分で用意する必要があります。

インストールするバージョンは Linuxbrew に合わせて `2.3.7` を使用します。
インストール先は、公式で推奨されている `/home/linuxbrew/.linuxbrew/` です。

````sh
$ curl -# -O https://cache.ruby-lang.org/pub/ruby/2.3/ruby-2.3.7.tar.bz2
$ tar xf ruby-2.3.7.tar.bz2
$ cd ruby-2.3.7
$ RUBY_VERSION="2.3.7"
$ PREFIX=/home/linuxbrew/.linuxbrew/Homebrew/Library/Homebrew/vendor/portable-ruby
$ INSTALL_DIR=${PREFIX}/${RUBY_VERSION}
$ ./configure --prefix=${INSTALL_DIR} --enable-load-relative --with-static-linked-ext --with-out-ext=tk,sdbm,gdbm,dbm --without-gmp --disable-install-doc --disable-install-rdoc --disable-dependency-tracking
$ make
$ make install
$ cd ${PREFIX}
$ ln -s ${RUBY_VERSION} current
$ PATH=${PREFIX}/current/bin:${PATH}
$ which ruby
$ ruby --version
````

### Linuxbrew のインストール

最後に linuxbrew をインストールします。
これも通常の手順と同じです。

````sh
$ sh -c "$(curl -fsSL https://raw.githubusercontent.com/Linuxbrew/install/master/install.sh)"
````

### 環境変数の設定

`brew` コマンドを使用するため、環境変数を設定します。
これも通常の手順と同じです。

````sh
$ echo 'export PATH="/home/linuxbrew/.linuxbrew/bin:${PATH}"' >> ~/.profile
$ . ~/.profile
````

### 環境の診断と修正

`brew doctor` を使って問題点がないか確認し、必要に応じて修正します。
私が試した際は、必要なディレクトリが作成されていませんでした。

````sh
$ brew doctor
$ sudo mkdir -p /home/linuxbrew/.linuxbrew/var/homebrew/linked
$ sudo chown -R $(whoami) /home/linuxbrew/.linuxbrew/var/homebrew/linked
$ brew doctor
Your system is ready to brew.
````

## 参考リンク

上記の手順は以下のコードをもとに、必要な作業を手動で行ったものです。

- https://raw.githubusercontent.com/Linuxbrew/install/master/install.sh
- https://raw.githubusercontent.com/Linuxbrew/install/master/install-ruby
- https://raw.githubusercontent.com/Linuxbrew/install/master/install
- https://github.com/Homebrew/homebrew-portable-ruby/blob/master/Formula/portable-ruby.rb

## その他

### 冗長出力

問題が発生した場合は、`HOMEBREW_VERBOSE` を設定すると冗長出力してくれます。

````sh
export HOMEBREW_VERBOSE=1
````

### OpenSSL のインストール

Linuxbrew の OpenSSL の formula は 64bit ARM Linux に対応していません。
そのため、`brew edit` で formula を修正してからインストールを行います。

ad-hoc な対応ですが、以下の `diff` のように、OpenSSL のプラットフォーム指定を追加することで、インストールが成功します。

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

## おわりに

以上、2018 年 11 月現在において、64bit ARM Linux 環境に Linuxbrew をインストールする手順をまとめました。

ご参考まで。