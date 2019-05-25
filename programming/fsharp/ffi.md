# FFI

`System.Runtime.InteropServices` と `DllImport` を使用するみたい。

## 注意

- 2.2.106 の FSI には FFI が使用できない問題がある
  - preview 版では直っています

## ライブラリの場所

### CoreFoundation

````
/System/Library/Frameworks/CoreFoundation.framework/CoreFoundation
````

### IOKit

````
/System/Library/Frameworks/IOKit.framework/IOKit
````

## 参考情報

- https://qiita.com/cannorin/items/59d79cc9a3b64c761cd4#pinvoke-ffi
- https://github.com/fsharp/fsharp/issues/886

### CoreFoundation
- https://gist.github.com/mauricio/2164127

### CoreFoundation and IOKit
- https://medium.com/@donblas/lets-bind-an-iokit-method-by-hand-fba939b54222
- http://aile.hatenablog.com/entry/advent20161225
