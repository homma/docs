
## Linuxbrew のインストール

### Portable Ruby について
- Linuxbrew の動作には Ruby が必要です
- Linuxbrew のインストール時に Portable Ruby と呼ばれる専用の処理系がインストールされます
- Portable Ruby は 32bit 版のため、Gemini の Debian では使用できません

- 対処方法として以下が考えられます
1. 32bit Ruby が動作するように、ライブラリやツールをインストールする
2. apt で Ruby をインストールする
3. docker-ce をインストールし docker 環境で Poratble Ruby をビルドする
4. ソースコードから Ruby をビルドし、ユーザのローカル環境にインストールする
5. 他の環境で Portable Ruby をビルドする

- 5 は具体的な方法を考えるのが大変
- 一番環境を汚さないのは、4 なので、ソースコードからビルドすることにします

### Linuxbrew に必要なソフトウェアのインストール

以下のソフトウェアは Linuxbrew に必要なため、事前にインストールします。

````
$ sudo apt-get install build-essential curl file git
````

### Ruby のインストール

作業は `~/Applications/Ruby` で行います。  
インストールするバージョンは Linuxbrew に合わせて 2.3.7 にします。  

````
$ mkdir -p ~/Applications/Ruby
$ cd $_
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

#### 参考リンク
- https://www.ruby-lang.org/ja/news/2018/03/28/ruby-2-3-7-released/
- https://www.ruby-lang.org/ja/documentation/installation/#building-from-sourc

### Linuxbrew のインストール

インストール:
````sh
$ sh -c "$(curl -fsSL https://raw.githubusercontent.com/Linuxbrew/install/master/install.sh)"
````

環境変数の設定:
````sh
$ echo 'export PATH="/home/linuxbrew/.linuxbrew/bin:$PATH"' >>~/.profile
$ echo 'export MANPATH="/home/linuxbrew/.linuxbrew/share/man:$MANPATH"' >>~/.profile
$ echo 'export INFOPATH="/home/linuxbrew/.linuxbrew/share/info:$INFOPATH"' >>~/.profile
````

環境の診断と修正:
````sh
$ brew doctor
$ sudo mkdir -p /home/linuxbrew/.linuxbrew/var/homebrew/linked
$ sudo chown -R $(whoami) /home/linuxbrew/.linuxbrew/var/homebrew/linked
$ brew doctor
Your system is ready to brew.
````

#### 参考リンク
- http://linuxbrew.sh

## 参考リンク

- https://raw.githubusercontent.com/Linuxbrew/install/master/install.sh
- https://raw.githubusercontent.com/Linuxbrew/install/master/install-ruby
- https://raw.githubusercontent.com/Linuxbrew/install/master/install
- https://hub.docker.com/r/linuxbrew/portable/~/dockerfile/
- https://github.com/Homebrew/homebrew-portable-ruby/blob/master/Formula/portable-ruby.rb

## メモ

### Tips
`HOMEBREW_VERBOSE` を設定すると冗長出力してくれます。

````sh
export HOMEBREW_VERBOSE=1
````

### エラー

これは結局わからず。  
インストール方法を上の方法に変更したところ、出なくなりました。

````
==> Migrating HOMEBREW_REPOSITORY (please wait)...
/bin/bash: warning: setlocale: LC_ALL: cannot change locale (en_US.UTF-8)
Error: Failed to migrate HOMEBREW_REPOSITORY to /home/linuxbrew/.linuxbrew/Homebrew!
The error was:
  undefined method `link_completions_manpages_and_docs' for UpdateMigrator:Module
Please try to resolve this error yourself and then run `brew update` again to
complete the migration. If you need help please +1 an existing error or comment
with your new error in issue:
  https://github.com/Homebrew/brew/issues/987
/home/linuxbrew/.linuxbrew/Library/Homebrew/update_migrator.rb:361:in `migrate_legacy_repository_if_necessary'
/home/linuxbrew/.linuxbrew/Library/Homebrew/cmd/update-report.rb:147:in `update_report'
/home/linuxbrew/.linuxbrew/Library/Homebrew/brew.rb:89:in `<main>'
````

### Linuxbrew は aarch64 をサポートしていない
- http://linuxbrew.sh

````sh
$ sudo apt-get install build-essential curl file git
$ sh -c "$(curl -fsSL https://raw.githubusercontent.com/Linuxbrew/install/master/install.sh)"
````

64bit Arm はサポートされていませんでした。  
Gemini PDA の Debian には 32bit 用の ld が入っていないため、32bit 版の Ruby も使用できません。

最初のコマンドを実行すると、`/home/linuxbrew` ディレクトリが作成されます。

````sh
$ sudo mkdir -p /home/linuxbrew
$ sudo chown ${USER}: /home/linuxbrew
$ git clone https://github.com/Linuxbrew/brew.git ./.linuxbrew
$ vi ~/.profile
PATH=/home/linuxbrew/.linuxbrew/bin:${PATH}
export MANPATH=$(brew --prefix)/share/man:${MANPATH}
export INFOPATH=$(brew --prefix)/share/info:${INFOPATH}

$ . ~/.profile

$ PATH=/home/linuxbrew/.linuxbrew/bin:/home/linuxbrew/.linuxbrew/sbin:$PATH \
HOMEBREW_BUILD_BOTTLE=1 \
HOMEBREW_BUILD_FROM_SOURCE=1 \
HOMEBREW_FORCE_VENDOR_RUBY=1 \
HOMEBREW_NO_ANALYTICS=1 \
HOMEBREW_NO_AUTO_UPDATE=1

$ git clone --depth=1 https://github.com/Linuxbrew/brew /home/linuxbrew/.linuxbrew
$ brew analytics off
$ brew tap Homebrew/core
$ brew tap Homebrew/portable-ruby
````

