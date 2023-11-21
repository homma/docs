---
title: SwiftUI のプログラムを作成する
status: draft
author: homma
---

--------------------------------------------------------------------------------

## SwiftUI の構成

`SwiftUI` のアプリケーションには `App` が必要です  

`swiftc` のみでアプリケーションを作成する場合は `appDelegate` も必要になります  
macOS の場合は、`NSApplicationDelegateAdaptor` を使用して `appDelegate` を設定します  

### App
- https://developer.apple.com/documentation/swiftui/app

`App` には `SwiftUI` の `Scene` を格納する `body` が必要です  
`App` は初期化と実行を担当する `main()` メソッドがあります  
`@main` は `import SwiftUI` と同時に使用できないようなので削除します  

### NSApplicationDelegateAdaptor
- https://developer.apple.com/documentation/swiftui/nsapplicationdelegateadaptor

macOS の場合は、`NSApplicationDelegate` プロトコルに適合した delegate を作成して`App` の `appDelegate` に指定します

--------------------------------------------------------------------------------

## 最小規模の SwiftUI プログラム

### main.swift

````swift
import SwiftUI

struct MyApp: App {
  @NSApplicationDelegateAdaptor private var appDelegate: MyAppDelegate

  var body: some Scene {
    Window("My SwiftUI Window", id: "mainwin") {
      Text("My SwiftUI App")
    }
    .defaultSize(CGSize(width: 640, height: 480))
  }
}

class MyAppDelegate: NSObject, NSApplicationDelegate {
  func applicationDidFinishLaunching(_ notification: Notification) {
    NSApp.setActivationPolicy(.regular)
    NSApp.activate()
  }
}

MyApp.main()
````

`SWiftUI` の `Scene` を持つ `body` と `appDelegate` を用意して `MyApp` クラスを作成しました  

`MyApp` の `appDelegate` には delegate として `MyAppDelegate` を指定しました  

`MyAppDelegate` に `applicationDidFinishLaunching` メソッドを実装し、`activate()` することでアプリケーションをアクティベートします

最後に `main()` スタティックメソッドを呼び出してアプリケーションを起動します

### applicationDidFinishLaunching
- https://developer.apple.com/documentation/appkit/nsapplicationdelegate/1428385-applicationdidfinishlaunching

### NSApp
- https://developer.apple.com/documentation/appkit/nsapp

### setActivationPolicy
- https://developer.apple.com/documentation/appkit/nsapplication/1428621-setactivationpolicy
- https://developer.apple.com/documentation/appkit/nsapplication/activationpolicy/regular

### activate
- https://developer.apple.com/documentation/appkit/nsapplication/4168336-activate

### ビルドと実行

````sh
$ swiftc main.swift
$ ./main
````

--------------------------------------------------------------------------------
