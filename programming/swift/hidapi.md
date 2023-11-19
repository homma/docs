---
title: Swift で HIDAPI ライブラリを使用する
status: draft
author: homma
---

--------------------------------------------------------------------------------

### HIDAPI ライブラリについて

- https://github.com/libusb/hidapi

### HIDAPI ライブラリのインストール

Homebrew でインストールします

````sh
$ brew install hidapi
````

### 用意するファイル

以下のファイルが必要です

````
build.sh
main.swift
module.modulemap
umbrella.h
````

`umbrella.h` は `HIDAPI` ライブラリのヘッダーファイルを読み込むために必要となります  
`module.modulemap` は `HIDAPI` ライブラリを Swift から使用するためのモジュールの作成に必要となります  
`main.swift` は `HIDAPI` ライブラリを使用する Swift のプログラムです  
`build.sh` は実行ファイルをビルドするために使用します

### umbrella.h

`umbrella.h` で `HIDAPI` ライブラリのヘッダーファイルをインクルードします

````c
#include <hidapi.h>
#include <hidapi_darwin.h>
````

### module.modulemap

`module.modulemap` は `raylib` をモジュール化するために必要となります  
モジュール化することで Swift からライブラリを呼び出すことができるようになります

`umbrella header` で `"umbrella.h"` を指定します  
`link` で `"hidapi"` を指定し、`libhidapilib.dylib` がリンクされるようにします

````
module HIDAPI [system] {
  umbrella header "umbrella.h"
  link "hidapi"
  export *
}
````

### main.swift

`HIDAPI` を使用する Swift のプログラムです  
デバイスの一覧を出力するプログラムです

````swift
import HIDAPI

func wchar_to_string(_ string: UnsafeMutablePointer<Int32>?) -> String {
  guard let s = string else {
    return "nil"
  }
  let length = wcslen(s) + 1
  var buf = [CChar](repeating: 0, count: length * 4)
  wcstombs(&buf, s, length)

  return String(cString: buf)
}

func print_hid_device_info(device dev: hid_device_info) {
  let device_info = """
    == DEVICE ==
    path                 \(String(cString: dev.path))
    vendor_id            \(dev.vendor_id)
    product_id           \(dev.product_id)
    serial_number        \(wchar_to_string(dev.serial_number))
    release_number       \(dev.release_number)
    manufacturer_string  \(wchar_to_string(dev.manufacturer_string))
    product_string       \(wchar_to_string(dev.product_string))
    usage_page           \(dev.usage_page)
    usage                \(dev.usage)
    interface_number     \(dev.interface_number)

    """

  print(device_info)
}

func main() {
  hid_init()
  hid_darwin_set_open_exclusive(0)

  guard var dev = hid_enumerate(0, 0) else {
    exit(1)
  }

  while true {
    print_hid_device_info(device: dev.pointee)

    if let next = dev.pointee.next {
      dev = next
    } else {
      break
    }

  }
}

main()
````

`import HIDAPI` で `HIDAPI` のモジュールを呼び出します

### build.sh

````sh
#!/bin/sh -x

rm main
swiftc main.swift -I. -I $(brew --prefix hidapi)/include -L $(brew --prefix hidapi)/lib
````

`-I.` オプションでカレントディレクトリにある `module.modulemap` ファイルを読み込んでいます  
`-I` と `-L` オプションで Homebrew によりインストールされた `HIDAPI` ライブラリの位置を参照しています

`chmod +x build.sh` で実行権限を付与しておきます

### ビルド

````sh
$ ./build.sh
````

### 実行

````sh
$ ./main
````

--------------------------------------------------------------------------------
