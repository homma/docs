---
title: Swift から C の関数を呼び出す方法
status: draft
author: homma
---

--------------------------------------------------------------------------------

Swift から C の関数を呼び出す方法をまとめます

現状の Swift は C の関数を簡単に呼び出すことができます  
繋ぎのコードをほとんど書く必要がないため、とても便利です

--------------------------------------------------------------------------------

## libc の関数を呼び出す

以下は、`libc` の `sleep(3)`、`puts(3)` を呼び出す例です

### main.swift

`import Darwin` を記載すると `libc` の関数を呼び出せるようになります

````swift
import Foundation
import Darwin

puts(Date().description);
sleep(1);
puts(Date().description);
````

### 実行

````sh
$ swift ./main.swift
````

--------------------------------------------------------------------------------

### `import Darwin` について

以下の `modulemap` に定義されているので import できるものと思われます

````
/Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX.sdk/usr/include/module.modulemap 
````

### `modulemap` について

`modulemap` は Clang が提供している仕組みです  
C のライブラリをモジュール化して、Swift から呼び出せるようにすることができます

- https://clang.llvm.org/docs/Modules.html

--------------------------------------------------------------------------------

## ncurses の関数を呼び出す

`libc` の代わりに `ncurses` ライブラリの関数を呼び出します

### main.swift

`import Darwin.ncurses` で `ncurses` の関数を呼び出せるようになります

````swift
import Darwin.ncurses

initscr();
cbreak();
noecho();

let a = getch();
endwin();

print(a);
````

### 実行

````sh
$ swift ./main.swift
````

--------------------------------------------------------------------------------

### `import Darwin.ncurses` について

以下の `modulemap` に定義されているので import できるものと思われます

````
/Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX.sdk/usr/include/ncurses.modulemap
````

### C のマクロについて

定数のマクロについてはインポートされるようです

- https://github.com/apple/swift/blob/main/docs/HowSwiftImportsCAPIs.md

--------------------------------------------------------------------------------

## `modulemap` を作成して C ライブラリの関数を呼び出す

既存の `modulemap` が用意されていないライブラリを呼び出します

### ファイル

以下の三つのファイルを用意します

````
main.swift
umbrella.h
module.map
````

`main.swift` は `curses` ライブラリを呼び出す Swift のプログラムです  
`umbrella.h` は `curses` ライブラリのヘッダーファイルを参照するためのファイルです  
`module.map` は `curses` ライブラリをモジュール化して Swift から呼び出せるようにするファイルです  

`module.map` は `module.modulemap` と同じように扱われます

### umbrella.h

`curses` ライブラリのヘッダーファイルを参照するためのヘッダーファイルを用意します

````c
#include "curses.h"
````

### module.map 

`module.map` ファイルに `curses` という名前のモジュールを定義します

````
module curses [system] {
  umbrella header "umbrella.h"
  link "curses"
  export *
}
````

`link` に `curses` を記載して、`curses` ライブラリがリンクされるようにします  

`umbrella header` に `umbrella.h` へのパスを指定します  

パスは絶対パスまたは相対パスで指定する必要があり、環境変数などを読み込むことはできないようです  
そのため、`modulemap` ファイルをヘッダーファイルの近くに配置するか、長いパスを記載する必要があります  
今回は `umbrella.h` を用意し、ヘッダーファイルの近くに `module.map` を作成しています

`umbrella.h` を用意しない場合は、以下のように長いパスを記述した `module.map` ファイルを用意する必要があります  
パスの記述を間違えると、`redefinition of module` エラーが発生する場合があります  

````
module curses [system] {
  header "/Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX.sdk/usr/include/curses.h"
  link "curses"
  export *
}
````

`module.map` ファイルには C 言語風のコメントを含めることができます

### main.swift

`module.map` で定義した `curses` モジュールは `import curses` で呼び出すことができます

````swift
import curses

initscr();
cbreak();
noecho();

let a = getch();
endwin();

print(a);
````

### コンパイル

````sh
$ swiftc main.swift -I.
````

`modulemap` ファイルは `-I` オプションに指定したディレクトリから検索されるようです  
`-I.` オプションを指定すると、カレントディレクトリの `modulemap` ファイルを参照することができます

#### 実行

````sh
$ ./main
````

--------------------------------------------------------------------------------

## `umbrella.h` と `modulemap` について

`umbrella.h` に記述されたヘッダーファイルへの参照はコンパイラのインクルードパスが考慮された物になります  
そのため、簡潔にヘッダーファイルを指定することができます

`module.map` ファイルに記述されたヘッダーファイルは、コンパイラのインクルードパスが考慮されません  
そのため、絶対パスか相対パスを使用して、参照先のヘッダーファイルの位置を正確に記述する必要があります  

`module.map` だけでは記述が長くなり、実行環境にも依存した記述になってしまうため、`umbrella.h` を用意するのが便利だと思います

--------------------------------------------------------------------------------

## Package.swift を使用して C ライブラリの関数を呼び出す

`Package.swift` を用意するとビルド設定をファイルに保存することができます

### ファイル

以下の 3 つのファイルを用意します

````
./Package.swift
./Sources/curses/module.modulemap
./Sources/myapp/main.swift
````

`./Package.swift` はパッケージの設定ファイルです  
`./Sources/curses/module.modulemap` は C のライブラリを呼び出すためのマッピングファイルです  
`./Sources/myapp/main.swift` は C の関数を呼び出す Swift プログラムです  

`Package.swift` はパッケージのルートディレクトリに配置します  
それ以外は `Sources/<ターゲット名>` のディレクトリを作成して配置します

### セットアップ

必要なディレクトリとファイルを用意します  

````sh
$ mkdir proj
$ cd proj
$ touch Package.swift
$ mkdir -p Sources/myapp
$ touch Sources/myapp/main.swift
$ mkdir -p Sources/curses
$ touch Sources/curses/umbrella.h
$ touch Sources/curses/module.modulemap
````

### Package.swift

````swift
// swift-tools-version: 5.9

import PackageDescription

let package = Package(
  name: "myapp",
  products: [
    .executable(
      name: "myapp",
      targets: ["myapp"])
  ],
  targets: [
    .executableTarget(
      name: "myapp",
      dependencies: ["curses"]
    ),
    .systemLibrary(name: "curses")
  ]
)
````

実行ファイルを作成するため、`products` に `.executable` を記述します  
実行ファイルの作成に使用するターゲットは `.executableTarget` の `myapp` です  

`.executableTarget` の `myapp` は `.systemLibrary` の `curses` に依存しています  
`.systemLibrary` は使用するライブラリの内、システムにインストール済みの物に使用します

### Sources/curses/umbrella.h

`curses` ライブラリのヘッダーファイルを参照するヘッダーファイルを作成します

````c
#include "curses.h"
````

### Sources/curses/module.modulemap

`curses` ライブラリを使用するための `modulemap` です  
`umbrella.h` を参照しています  
`curses` ライブラリをリンクするため、`link` に `curses` を指定しています

````
module curses [system] {
  umbrella header "umbrella.h"
  link "curses"
  export *
}
````

`Package.swift` を使用する場合のファイル名は `module.map` ではなく `module.modulemap` にする必要があります

### main.swift

````swift
import curses

initscr();
cbreak();
noecho();

let a = getch();
endwin();

print(a);
````

### ビルドと実行

````sh
$ swift build -c release
$ swift run --skip-build
````

または

````sh
$ swift run -c release
````

`swift build` コマンドはパッケージをビルドします
`swift run --skip-build` コマンドはビルドされたプログラムを実行します

`swift run` コマンドは build してからプログラムを実行します  
`-c release` オプションを付けないとデバッグビルドになります

--------------------------------------------------------------------------------

## Package.swift について

- https://docs.swift.org/package-manager/PackageDescription/PackageDescription.html

`Package.swift` は、普通の Swift のプログラムです  
そのため、以下のように記述することもできます

````swift
// swift-tools-version: 5.9

import PackageDescription

let name = "myapp";

let p0 = Product.executable(name: "myapp", targets: ["myapp"]);
let products = [ p0 ];

let t0 = Target.executableTarget(name: "myapp",
                                 dependencies: ["curses"],
                                 linkerSettings: [
                                   .linkedLibrary("curses")
                                 ])
let t1 = Target.systemLibrary(name: "curses");
let targets = [ t0, t1 ];

let package = Package(name: name, products: products, targets: targets);
````

--------------------------------------------------------------------------------

## Homebrew でインストールした C ライブラリの関数を呼び出す

例として、Swift から `hidapi` ライブラリの関数を呼び出します

### ファイル

以下のようなファイル構成を使用します

````
./Package.swift
./Sources/myapp/main.swift
./Sources/hidapi/umbrella.h
./Sources/hidapi/module.modulemap
````

### セットアップ

あらかじめ Homebrew および Swift から呼び出したい C ライブラリをインストールしておきます

````sh
$ brew install hidapi
````

必要なディレクトリとファイルを用意します

````sh
$ mkdir proj
$ cd proj
$ touch Package.swift
$ mkdir -p Sources/myapp
$ touch Sources/myapp/main.swift
$ mkdir -p Sources/hidapi
$ touch Sources/hidapi/umbrella.h
$ touch Sources/hidapi/module.modulemap
````

### Package.swift

`Package.swift` ファイルにビルドの設定を記述します

````swift
// swift-tools-version: 5.9

import PackageDescription

let package = Package(
  name: "myapp",
  products: [
    .executable(
      name: "myapp",
      targets: ["myapp"])
  ],
  targets: [
    .executableTarget(
      name: "myapp",
      dependencies: ["hidapi"]),
    .systemLibrary(
      name: "hidapi",
      pkgConfig: "hidapi",
      providers: [
        .brew(["hidapi"])
      ])
  ]
)
````

`pkgConfig` と `providers` により、Homebrew でインストールした `hidapi` ライブラリが参照されます

### Sources/myapp/main.swift

````swift
import hidapi

hid_init()
hid_darwin_set_open_exclusive(0)
````

### Sources/hidapi/umbrella.h

`umbrella.h` は参照するライブラリを使用するために必要となるヘッダーファイルをまとめたものです

````c
#include "hidapi.h"
#include "hidapi_darwin.h"
````

### Sources/hidapi/module.modulemap

`modulemap` からは `umbrella.h` を参照します

````
module hidapi [system] {
  umbrella header "umbrella.h"
  export *
}
````

### ビルドと実行

````sh
$ swift build -c release
$ swift run --skip-build
````

または

````sh
$ swift run -c release
````

--------------------------------------------------------------------------------

## Homebrew でインストールした C ライブラリを含むパッケージを作成する

例として、Swift から `hidapi` ライブラリの関数を呼び出します

### ファイル

以下のようなファイル構成を使用します

````
./Package.swift
./Sources/myapp/main.swift
./Sources/hidapi/module.modulemap
./Sources/hidapi/<hidapi のファイル一式>
````

### セットアップ

あらかじめ Homebrew および Swift から呼び出したい C ライブラリをインストールしておきます

````sh
$ brew install hidapi
````

必要なディレクトリとファイルを用意します

````sh
$ mkdir proj
$ cd proj
$ touch Package.swift
$ mkdir -p Sources/myapp
$ touch Sources/myapp/main.swift
$ mkdir -p Sources/hidapi
$ cd Sources/hidapi
$ touch module.modulemap
$ cp -Rp (brew --prefix hidapi)/ .
````

### Package.swift

````swift
// swift-tools-version: 5.9

import PackageDescription

let package = Package(
  name: "myapp",
  products: [
    .executable(name: "myapp",
                targets: ["myapp"])
  ],
  targets: [
    .executableTarget(name: "myapp",
                      dependencies: ["hidapi"],
                      linkerSettings: [
                        .linkedLibrary("hidapi"),
                        .unsafeFlags(["-LSources/hidapi/lib"])
                      ]),
    .systemLibrary(name: "hidapi",
                   providers: [
                     .brew(["hidapi"])
                   ])
  ]
)
````

### Sources/myapp/main.swift

````swift
import hidapi

hid_init()
hid_darwin_set_open_exclusive(0)
````

### Sources/hidapi/module.modulemap

````
module hidapi [system] {
  header "include/hidapi/hidapi.h"
  header "include/hidapi/hidapi_darwin.h"
  link "lib/libhidapi.dylib"
  export *
}
````

### ビルド

````sh
$ swift build -c release
````

--------------------------------------------------------------------------------
