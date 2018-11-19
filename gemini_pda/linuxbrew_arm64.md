
## 64bit ARM Linux で Linuxbrew を使用する際の Tips

### Hardware::CPU.arch_64_bit

`arm64` が定義されています。

https://github.com/Linuxbrew/brew/blob/master/Library/Homebrew/hardware.rb
````
      def arch_64_bit
        if arm?
          :arm64
````
