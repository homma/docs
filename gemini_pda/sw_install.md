
## 追加ソフトウェアのインストール

### linuxbrew
- http://linuxbrew.sh

````sh
$ sudo apt-get install build-essential curl file git
$ sh -c "$(curl -fsSL https://raw.githubusercontent.com/Linuxbrew/install/master/install.sh)"
````

64bit Arm はサポートされていませんでした。  
Gemini PDA の Debian には 32bit 用の ld が入っていないため、32bit 版の Ruby も使用できません。

#### インストールメモ

以下を参考にして手動でインストールします。

- https://raw.githubusercontent.com/Linuxbrew/install/master/install.sh
- https://raw.githubusercontent.com/Linuxbrew/install/master/install-ruby
- https://raw.githubusercontent.com/Linuxbrew/install/master/install

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

$ PREFIX=/home/linuxbrew/.linuxbrew/Library/Homebrew/vendor
$ curl -L https://homebrew.bintray.com/bottles-portable-ruby/portable-ruby-2.3.7.armv6_linux.bottle.tar.gz | tar -xz -C ${PREFIX}
````

### git
- デフォルトではインストールされていない
- linuxbrew のインストール時にインストール

### 端末エミュレータ

### 日本語環境を整える
- mozc

### nodebrew
- https://github.com/hokaccha/nodebrew

````sh
$ curl -L git.io/nodebrew | perl - setup
````

### node.js
- nodebrew を使う

### Lem

````sh
$ brew search roswell
````

### Electron 動作確認
