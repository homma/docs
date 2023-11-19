---
title: Swift で Raylib のプログラムを作成する
status: draft
author: homma
---

--------------------------------------------------------------------------------

### Raylib のインストール

Homebrew でインストールします

````sh
$ brew install raylib
````

### 用意するファイル

以下のファイルが必要です

````
build.sh
main.swift
module.modulemap
umbrella.h
````

`umbrella.h` は `Raylib` のヘッダーファイルを読み込むために必要となります  
`module.modulemap` は `Raylib` ライブラリを Swift から使用するためのモジュールを作成するために必要となります  
`main.swift` は `Raylib` ライブラリを使用する Swift のプログラムです  
`build.sh` は実行ファイルをビルドするために使用します

### umbrella.h

`umbrella.h` で `Raylib` のヘッダーファイルをインクルードします

````c
#include "raylib.h"
#include "raymath.h"
#include "rlgl.h"
````

### module.modulemap

`Raylib` をモジュール化するために必要となります  
`umbrella header` で `"umbrella.h"` を指定します  
`link` で `"raylib"` を指定し、`libraylib.dylib` がリンクされるようにします

````
module raylib [system] {
  umbrella header "umbrella.h"
  link "raylib"
  export *
}
````

### main.swift

````swift
import raylib

func update() {
  BeginDrawing()

  let white = Color(r: 255, g: 255, b: 255, a: 255)
  ClearBackground(white)

  let x:Int32 = 110
  let y:Int32 = 170
  let h:Int32 = 30
  let c = Color(r: 0, g: 0, b: 0, a: 255)
  let t = "This is a raylib window"

  DrawText(t, x, y, h, c)

  EndDrawing()
}

func main() {
  let window = (w: Int32(600), h: Int32(400), title: "My Raylib Window")
  InitWindow(window.w, window.h, window.title)

  while WindowShouldClose() == false {
    update()
  }

  CloseWindow()
}

main()
````

`import raylib` で `raylib` のモジュールを呼び出します

`Color(r:, g:, b:, a:` は Raylib が用意している構造体です  
Swift からは `Color(r: 255, g: 255, b: 255, a: 255)` のように呼び出せます

それ以外は通常の `Raylib` の使い方と変わりありません

### build.sh

````
#!/bin/sh -x

swiftc main.swift -I. -I $(brew --prefix raylib)/include -L $(brew --prefix raylib)/lib
````

`-I.` オプションでカレントディレクトリにある `module.modulemap` ファイルを読み込んでいます  
`-I` と `-L` オプションで Homebrew によりインストールされた `Raylib` ライブラリの位置を参照しています

`chmod +x build.sh` で実行権限を付与しておきます

### ビルド

````
$ ./build.sh
````

### 実行

````
$ ./main
````

--------------------------------------------------------------------------------
