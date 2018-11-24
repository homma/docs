
## 64bit ARM Linux で Linuxbrew を使用する際の Tips

### Hardware::CPU.arch_64_bit

`arm64` が定義されています。

https://github.com/Linuxbrew/brew/blob/master/Library/Homebrew/hardware.rb
````
      def arch_64_bit
        if arm?
          :arm64
````

### そのままではインストールができないソフトウェア

#### OpenSSL

- [linuxbrew_openssl.md](./linuxbrew_openssl.md) 参照
- ワークアラウンド確認済み

#### rxvt-unicode

- configure に -march=native が含まれているため失敗
- aarch64 は GCC v8 で -march=native に対応

https://www.phoronix.com/scan.php?page=news_item&px=GCC-8-march-native-ARM

- Linuxbrew の GCC は v5
- Debian 9 の GCC は v6

- ワークアラウンドは [linuxbrew_march.md](./linuxbrew_march.md)

#### zsh

- configure に -march=native が含まれているため失敗
- rxvt-unicode と同じ状況

- ワークアラウンドは [linuxbrew_march.md](./linuxbrew_march.md)

#### w3m

- 依存ライブラリ (libatomic_ops) の configure に -march=native が含まれているため失敗

- ワークアラウンドは [linuxbrew_march.md](./linuxbrew_march.md)
