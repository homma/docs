---
title: Swift から C の関数を呼び出す方法
status: draft
---

--------------------------------------------------------------------------------

## libc の関数を呼び出す

`libc` の `sleep(3)` を呼び出す例

### main.swift

`import Darwin` を記載すると `libc` の関数を呼び出せるようになります

````swift
import Foundation
import Darwin

print(Date());
sleep(1);
print(Date());
````

### 実行

````sh
$ swift ./main.swift
````

### `import Darwin` について

以下の `modulemap` に定義されているので import できるものと思われます

- /Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX.sdk/usr/include/module.modulemap 

### `modulemap` について

`modulemap` は Clang が提供している仕組みです

- https://clang.llvm.org/docs/Modules.html

--------------------------------------------------------------------------------

## ncurses の関数を呼び出す

`libc` の代わりに `ncurses` の関数を呼び出します

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

### `import Darwin.ncurses` について

以下の `modulemap` に定義されているので import できるものと思われます

- /Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX.sdk/usr/include/ncurses.modulemap

--------------------------------------------------------------------------------

## modulemap を使用して C ライブラリの関数を呼び出す

`modulemap` が用意されていないライブラリを呼び出します

### ファイル

以下の二つのファイルを用意します

````
main.swift
module.map
````

`module.map` は `module.modulemap` と同じように扱われます

### module.map 

`module.map` ファイルに `curses` という名前のモジュールを定義します

`header` にヘッダーファイルのパスを指定します  
`link` にライブラリのパスを指定します  

パスは絶対パスまたは相対パスで指定する必要があり、環境変数などを読み込むことはできないようです  
ファイルには C 言語風のコメントを含めることができます

````
module curses [system] {
  header "/Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX.sdk/usr/include/curses.h"
  link "/Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX.sdk/usr/lib/libcurses.tbd"
  export *
}
````

`header` に `/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk` 以下のヘッダーファイルを指定すると `redefinition of module` エラーが発生します

### main.swift

`module.map` で定義した `curses` モジュールは `import curses` で呼び出すことができます

````swift
import curses

initscr()
cbreak();
noecho();

let a = getch();
endwin();

print(a);
````

### コンパイル

````sh
$ swiftc main.swift -I. -lcurses
````

`-lcurses` でリンクしないと `ld: warning: Could not find or use auto-linked library` エラーが発生します

#### 実行

````sh
$ ./main
````

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

`./Package.swift` はパッケージの設定ファイル  
`./Sources/curses/module.modulemap` は C のライブラリを呼び出すためのマッピングファイル  
`./Sources/myapp/main.swift` は C の関数を呼び出す Swift プログラム  

`Package.swift` はパッケージのルートディレクトリに配置します  
それ以外は `Sources/<ターゲット名>` のディレクトリを作成して配置します

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
                      dependencies: ["curses"],
                      linkerSettings: [
                        .linkedLibrary("curses")
                      ]),
    .systemLibrary(name: "curses")
  ]
)
````

実行ファイルを作成するため、`products` に `.executable` を記述します  
実行ファイルの作成に使用するターゲットは `.executableTarget` の `myapp` です  

`.executableTarget` の `myapp` は `.systemLibrary` の `curses` に依存しています  
`linkerSettings` で `curses` をリンクします  

`.systemLibrary` は使用するライブラリの内、システムにインストール済みの物に使用します

### module.modulemap

`curses` ライブラリを使用するための `modulemap` です  
この場合のファイル名は `module.map` ではなく `module.modulemap` にする必要があります

````
module curses [system] {
  header "/Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX.sdk/usr/include/curses.h"
  link "/Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX.sdk/usr/lib/libcurses.tbd"
  export *
}
````

### main.swift

````swift
import curses

initscr()
cbreak();
noecho();

let a = getch();
endwin();

print(a);
````

### build command

````sh
$ swift build
````

または

````sh
$ swift run
````

`swift build` は build するだけです  
`swift run` の場合は build してからプログラムを実行します

## Package.swift について

- https://docs.swift.org/package-manager/PackageDescription/PackageDescription.html

ただの Swift のプログラムなので、以下のように記述することもできます

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

`cp` でコピーする代わりにシンボリックリンクを作成することもできます

````
$ ln -s $(brew --prefix hidapi)/include .
$ ln -s $(brew --prefix hidapi)/lib .
````

ビルドしたものを配布するのであればコピー、自分で使用するだけであればリンクが良いかもしれません  
コピーもリンクもしたくない場合は、Homebrew でインストールした場所を modulemap や Package.swift にそのまま記載します  

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
                      ]
                      ),
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

### build

````sh
$ swift build
````

または

````sh
$ swift run
````

--------------------------------------------------------------------------------
