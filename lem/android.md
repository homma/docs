Lem: Android へのインストール

Android タブレットで Lem を動作させることができました。  
起動には 30 秒弱かかりますが、起動してしまえば普通に動きます。

### 注意
- Roswell のインストールに `sudo` を使用するようになっていますが、ユーザのホームディレクトリにインストールした方が良さそう
- 内蔵ストレージに 2GB 程度の空き容量が必要（SD Card にはインストールできない）
- 実行は自己責任で

### インストール手順
`Termux -> PRoot + Arch Linux -> Roswell -> Clozure CL -> Lem` の順にインストールします。

Termux のインストール
- 省略

Arch Linux のインストール
````sh
$ pkg install git
$ git clone https://github.com/sdrausty/TermuxArch
$ bash TermuxArch/setupTermuxArch.sh
````

パッケージのインストール
````sh
$ pacman -S --needed base-devel
$ pacman -S git
````

ユーザの追加
````sh
// Arch Linux を抜ける
$ exit
$ setupTermuxArch.sh c addauser me
$ passwd me
// Arch Linux を抜ける
$ exit
````

`sudo` を許可
````sh
$ startarch
$ visudo
// me user を許可
$ passwd me
````

作成したユーザでログイン
````sh
$ startarch -l me
$ sudo ls
````

Roswell のインストール
````sh
$ git clone -b release https://github.com/roswell/roswell.git
$ cd roswell
$ sh bootstrap
$ ./configure
$ make
$ sudo make install
````

Clozure CL のインストール
````sh
$ ros install ccl-bin
$ ros use ccl-bin
````

Lem のインストール
````sh
$ ros install cxxxr/lem
$ export PATH=${PATH}:${HOME}/.roswell/bin
$ lem --frontend ncurses-ccl
````

### AUR の Roswell は ARM に対応していない
設定ファイルを書き換えればビルドできそうな気もしますが、試していません。

````sh
$ mkdir ~/pkg; cd $_
$ git clone https://aur.archlinux.org/roswell.git
$ cd roswell
$ makepkg -si
ERROR: roswell is not available for the 'armv7h' architecture.
````

- https://aur.archlinux.org/packages/roswell/
- https://wiki.archlinux.org/index.php/Arch_User_Repository

### `sudo` について
- https://github.com/sdrausty/termux-archlinux/issues/4

### SBCL では起動せず
`Ptr ... sees non-Lisp memory` というメッセージが表示されます。
