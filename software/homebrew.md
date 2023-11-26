
## Homebrew メモ

### Homebrew non-root install
しばらくは Homebrew を使うことにしました

````sh
$ cd
$ mkdir <installdir>/.homebrew
$ ln -s <installdir>/.homebrew .
$ curl -L https://github.com/Homebrew/brew/tarball/master | tar xz --strip 1 -C .homebrew
$ eval "$(.homebrew/bin/brew shellenv)"
$ brew update --force --quiet
$ chmod -R go-w "$(brew --prefix)/share/zsh"
$ echo 'export PATH=${HOME}/.homebrew/bin:${PATH}' >> ~/.zshrc
````

### パッケージのインストールの際にコンパイルが発生する場合がある

デフォルトのパスと異なるため、コンパイルが実行される

````sh
% brew install rsync
...
Warning: Building openssl@3 from source as the bottle needs:
...
built in 4 minutes 59 seconds
````

`brew install ripgrep` の場合はコンパイルは必要なかった

頻繁にインストール、アップグレードを実行するわけではないので、しばらくはこれで使ってみる

### コンパイル発生の有無

#### コンパイルが発生したパッケージ
- rsync
   - openssl のコンパイルが必要
- pkg-config

#### コンパイルが発生しなかったパッケージ
- ripgrep
- jq
- stylua
- luajit
- hidapi
- raylib
- swift-format
- sdl2
- gh

#### 未インストール
- Neovim
   - 別途インストールしたため

### macOS で使用可能なパッケージシステム
non-root インストールしたい  
できれば Nixpkgs を使いたいところですが...

#### Homebrew
https://github.com/Homebrew/brew

non-root インストールは可能であるがサポートされていない

https://docs.brew.sh/Installation#untar-anywhere-unsupported  
https://github.com/Homebrew/brew/blob/master/docs/Installation.md#untar-anywhere-unsupported

#### Nixpkgs
https://github.com/nixos/nixpkgs

Linux では `nix-user-chroot` を使用することで non-root インストールも可能  
macOS では `nix-user-chroot` は使用できなさそう

https://nixos.wiki/wiki/Nix_Installation_Guide#Installing_without_root_permissions

`proot` や `nix-portable` を使用する方法も macOS では使用できないっぽい

#### MacPorts
https://www.macports.org

non-root インストール可能  
`--with-no-root-privileges` オプションを使用するみたい

https://gist.github.com/zadr/e9a090fb564451fb5c76db233fabf0d0  
https://gist.github.com/daggerok/d6c7ed8b9efa03b30ffd0e9f44cdd121

#### Fink
https://www.finkproject.org

dpkg ベース

#### pkgsrc
https://www.pkgsrc.org

non-root インストール可能  
`--unprivileged` オプションでユーザのホームディレクトリにインストールできる

https://www.netbsd.org/docs/pkgsrc/platforms.html

release は quarterly

https://www.pkgsrc.org/quarterly/
