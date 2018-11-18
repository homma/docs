
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
- https://hub.docker.com/r/linuxbrew/portable/~/dockerfile/

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
