---
title: Swift で ncurses のプログラムを作成する
status: draft
author: homma
---

--------------------------------------------------------------------------------

ncurses は modulemap が作成されており、デフォルトの状態で swift から利用可能です

````
/Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX.sdk/usr/include/ncurses.modulemap
````

--------------------------------------------------------------------------------

## ncurses を使用した Swift プログラム

### main.swift

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
$ swift main.swift
````

--------------------------------------------------------------------------------
