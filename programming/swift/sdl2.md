---
title: Swift で SDL 2.0 のプログラムを作成する
status: draft
author: homma
---

--------------------------------------------------------------------------------

### SDL について

- https://www.libsdl.org

### SDL のインストール

Homebrew でインストールします

````sh
$ brew install sdl2
````

### 用意するファイル

以下のファイルが必要です

````
build.sh
main.swift
module.modulemap
umbrella.h
````

`umbrella.h` は `SDL` のヘッダーファイルを読み込むために必要となります  
`module.modulemap` は `SDL` ライブラリを Swift から使用するためのモジュールの作成に必要となります  
`main.swift` は `SDL` ライブラリを使用する Swift のプログラムです  
`build.sh` は実行ファイルをビルドするために使用します

### umbrella.h

`umbrella.h` で `SDL` のヘッダーファイルをインクルードします

````c
#include <SDL.h>
````

### module.modulemap

`module.modulemap` は `SDL` ライブラリをモジュール化するために必要となります  
モジュール化することで Swift からライブラリを呼び出すことができるようになります

`umbrella header` で `"umbrella.h"` を指定します  
`link` で `"SDL2"` を指定し、`libSDL2.dylib` がリンクされるようにします

````
module SDL2 [system] {
  umbrella header "umbrella.h"
  link "SDL2"
  export *
}
````

### main.swift

`SDL` を使用する Swift のプログラムです  
ウィンドウを開くだけの簡単なプログラムです  

````swift
import SDL2

func main() {

  if SDL_Init(SDL_INIT_VIDEO) < 0 {
    print("error")
    exit(1)
  }

  let window = (
    title: "My SDL2 Window",
    x: Int32(SDL_WINDOWPOS_CENTERED_MASK),
    y: Int32(SDL_WINDOWPOS_CENTERED_MASK),
    w: Int32(480),
    h: Int32(320),
    flags: UInt32(0)
  )
  let win = SDL_CreateWindow(
    window.title,
    window.x, window.y,
    window.w, window.h,
    window.flags
  )

  var event = SDL_Event()
  var quit = false

  while quit != true {
    while SDL_PollEvent(&event) != 0 {
      if event.type == SDL_QUIT.rawValue {
        SDL_DestroyWindow(win)
        SDL_Quit()
        quit = true
      }
    }
  }
}

main()
````

`import SDL2` で `SDL` のモジュールを呼び出します

### build.sh

````sh
#!/bin/sh -x

rm main
swiftc main.swift -I. -I $(brew --prefix sdl2)/include/SDL2 -L $(brew --prefix sdl2)/lib
````

`-I.` オプションでカレントディレクトリにある `module.modulemap` ファイルを読み込んでいます  
`-I` と `-L` オプションで Homebrew によりインストールされた `SDL` ライブラリの位置を参照しています

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
