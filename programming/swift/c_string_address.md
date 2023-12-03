---
title: C 文字列のアドレスを配列に格納する
status: draft
author: homma
---

--------------------------------------------------------------------------------

`posix_spawn(2)` で使用するため、C の文字列を配列に格納したいという要件がありました  
Swift 文字列の配列 `[String]` から C 文字列のアドレスの配列 `[UnsafeMutablePointer<CChar>]` を作成する必要がありました  

`for` 文を使用して Swift の文字列から C 文字列のアドレスを取り出そうとすると、ループが回るとアドレスがスコープから外れて自動的に削除されてしまい、C 文字列のアドレスの配列を取り出して保持することができませんでした  

````swift
// これは上手くいかない

import Foundation

let str = ["foo", "bar", "baz"]
var cs : [UnsafeMutablePointer<CChar>?] = []

for s in str {
  var buf = Array(repeating: CChar(0), count: s.count + 1)
  (s as NSString).getCString(&buf, maxLength: buf.count, encoding: NSUTF8StringEncoding)
  cs.append(&buf)
}

puts(cs[0])
````

`malloc` を使って C 側でアドレスを確保することも可能だと思いますが、メモリ管理を自前で行う必要が発生するため、可能な限り Swift 側で実装したいと考えました  

--------------------------------------------------------------------------------

### 作成した class

以下の class を作成することで、C 言語の文字列のアドレスを取り出しつつ、Swift 側のメモリ管理の恩恵も受けることができるようになりました  

````swift
import Foundation

class CStringBuffer {
  var cString: [CChar]
  var address = UnsafeMutableBufferPointer<CChar>(_empty: ())

  init(_ str: String) {
    // allocate a buffer
    var buf = Array(repeating: CChar(0), count: str.count + 1)

    // copy cString in str into buf
    (str as NSString).getCString(
      &buf, maxLength: buf.count,
      encoding: NSUTF8StringEncoding)

    self.cString = buf
    self.cString.withUnsafeMutableBufferPointer { ptr in
      self.address = ptr
    }
  }
}
````

`init` の中では、まず、`buf` 変数に C 文字列用のバッファを確保しています  
次に `getCString` でバッファに文字列を C の文字列としてコピーしています  

続いて `buf` をインスタンス変数として保存しています  
`self.cString` は Swift の配列のため、Swift 側でメモリが管理されます  

最後にバッファのアドレスを `self.address` に格納し、外部からアクセス可能にしています

--------------------------------------------------------------------------------

### 使用例

C 文字列のアドレスの配列が作りたかったので、`cstr` を `[UnsafeMutablePointer<CChar>?]` として確保しています  
`CStringBuffer` オブジェクトを保持するため、`keep` を別途作成しています  

````swift
import Foundation

class CStringBuffer {
  var cString: [CChar]
  var address = UnsafeMutableBufferPointer<CChar>(_empty: ())

  init(_ str: String) {
    // allocate a buffer
    var buf = Array(repeating: CChar(0), count: str.count + 1)

    // copy cString in str into buf
    (str as NSString).getCString(
      &buf, maxLength: buf.count,
      encoding: NSUTF8StringEncoding)

    self.cString = buf
    self.cString.withUnsafeMutableBufferPointer { ptr in
      self.address = ptr
    }
  }
}

func test() {
  let str = ["foo", "bar", "baz"]

  var keep: [CStringBuffer] = []
  var cstr: [UnsafeMutablePointer<CChar>?] = []

  for s in str {
    let cs = CStringBuffer(s)

    keep.append(cs)
    cstr.append(cs.address.baseAddress)
  }

  // direct access to cString
  puts(keep[0].cString)
  puts(keep[1].cString)
  puts(keep[2].cString)

  // access via cString address
  puts(cstr[0])
  puts(cstr[1])
  puts(cstr[2])
}

test()
````
