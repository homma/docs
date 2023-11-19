---
title: Swift で raylib のプログラムを作成する
status: draft
author: homma
---

--------------------------------------------------------------------------------

### raylib について

- https://www.raylib.com

ゲームの作成に使用されるシンプルで使いやすいライブラリです  
2D や 3D のイメージを表示したり、入力イベントを処理したり、サウンドを出力したりすることができます  
C 言語で実装されています

### raylib のインストール

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

`umbrella.h` は `raylib` のヘッダーファイルを読み込むために必要となります  
`module.modulemap` は `raylib` ライブラリを Swift から使用するためのモジュールの作成に必要となります  
`main.swift` は `raylib` ライブラリを使用する Swift のプログラムです  
`build.sh` は実行ファイルをビルドするために使用します

### umbrella.h

`umbrella.h` で `raylib` のヘッダーファイルをインクルードします

````c
#include "raylib.h"
#include "raymath.h"
#include "rlgl.h"
````

### module.modulemap

`module.modulemap` は `raylib` をモジュール化するために必要となります  
モジュール化することで Swift からライブラリを呼び出すことができるようになります

`umbrella header` で `"umbrella.h"` を指定します  
`link` で `"raylib"` を指定し、`libraylib.dylib` がリンクされるようにします

````
module Raylib [system] {
  umbrella header "umbrella.h"
  link "raylib"
  export *
}
````

### main.swift

`raylib` を使用する Swift のプログラムです  
ウィンドウを開いて文字列を表示するだけの簡単なプログラムです  

````swift
import Raylib

func update() {
  BeginDrawing()

  let white = Color(r: 255, g: 255, b: 255, a: 255)
  ClearBackground(white)

  let x: Int32 = 110
  let y: Int32 = 170
  let h: Int32 = 30
  let black = Color(r: 0, g: 0, b: 0, a: 255)
  let t = "This is a raylib window"

  DrawText(t, x, y, h, black)

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

`import Raylib` で `raylib` のモジュールを呼び出します

`Color(r:, g:, b:, a:)` は `Raylib` ライブラリが用意している C の構造体です  
Swift からは `Color(r: 255, g: 255, b: 255, a: 255)` のように呼び出せます

それ以外は通常の `raylib` の使い方と変わりありません

### build.sh

````sh
#!/bin/sh -x

rm main
swiftc main.swift -I. -I $(brew --prefix raylib)/include -L $(brew --prefix raylib)/lib
````

`-I.` オプションでカレントディレクトリにある `module.modulemap` ファイルを読み込んでいます  
`-I` と `-L` オプションで Homebrew によりインストールされた `raylib` ライブラリの位置を参照しています

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
