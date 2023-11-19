---
title: Swift で SQLite3 のプログラムを作成する
status: draft
author: homma
---

--------------------------------------------------------------------------------

sqlite3 は modulemap が作成されており、デフォルトの状態で swift から利用可能です

````
/Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX.sdk/usr/include/module.modulemap
````

--------------------------------------------------------------------------------

SQLite3 ライブラリには `import SQLite3` でアクセスすることができます

### main.swift
````
import SQLite3

if let version = sqlite3_libversion() {
  print(String(cString: version))
}
````

### 実行

````
$ swift main.swift
````

--------------------------------------------------------------------------------
