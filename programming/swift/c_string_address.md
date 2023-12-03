---
title: C 文字列のアドレスを配列に格納する
status: draft
author: homma
---

--------------------------------------------------------------------------------

`posix_spawn(2)` で使用するため、C の文字列を配列に格納したいという要件がありました  
Swift 文字列の配列 `[String]` から C 文字列のアドレスの配列 `[UnsafeMutablePointer<CChar>]` を作成する必要がありました  

`malloc` を使って C 側でアドレスを確保することも可能だと思いますが、メモリ管理を自前で行う必要が発生するため、可能な限り Swift 側で実装したいと考えました  

試してみたところ、以下のようにポインターを取得しつつ、バッファの参照も別途保存することで実現することができました  

````swift
import Foundation

let str = ["foo", "bar", "baz"]

var keep: [[CChar]] = []
var cstr: [UnsafeMutablePointer<CChar>?] = []

for s in str {
  // allocate a buffer
  var buf = Array(repeating: CChar(0), count: s.count + 1)

  // copy C string in s into buf
  (s as NSString).getCString(&buf, maxLength: buf.count, encoding: NSUTF8StringEncoding)

  // save the pointer
  buf.withUnsafeMutableBufferPointer { ptr in
    cstr.append(ptr.baseAddress)
  }

  // save the array object
  keep.append(buf)
}

puts(cstr[0])
````

--------------------------------------------------------------------------------

### 作成した class

必要な処理を抜き出して class を作成します  
以下の class を作成することで、C 言語の文字列のアドレスを取り出しつつ、Swift 側のメモリ管理の恩恵も受けることができるようになりました  

````swift
import Foundation

class CStringBuffer {
  var cString: [CChar]
  var address = UnsafeMutablePointer<CChar>(nil)

  init(_ str: String) {
    // allocate a buffer
    var buf = Array(repeating: CChar(0), count: str.count + 1)

    // copy C string in str into buf
    (str as NSString).getCString(
      &buf, maxLength: buf.count,
      encoding: NSUTF8StringEncoding)

    // save the array object
    self.cString = buf

    // save the pointer
    self.cString.withUnsafeMutableBufferPointer { ptr in
      self.address = ptr.baseAddress
    }
  }
}
````

`init` の中では、まず、`buf` 変数に C 文字列用のバッファを確保しています  
次に `getCString` でバッファに文字列を C の文字列としてコピーしています  

続いて `buf` をインスタンス変数として保存しています  
`self.cString` は Swift の配列のため、Swift 側でメモリが管理されます  

最後にバッファのベースアドレスを `self.address` に格納し、外部からアクセス可能にしています

--------------------------------------------------------------------------------

### 使用例

C 文字列のアドレスの配列が作りたかったので、`cstr` を `[UnsafeMutablePointer<CChar>?]` として確保しています  
`CStringBuffer` オブジェクトを保持するため、`keep` を別途作成しています  

````swift
import Foundation

class CStringBuffer {
  var cString: [CChar]
  var address = UnsafeMutablePointer<CChar>(nil)

  init(_ str: String) {
    // allocate a buffer
    var buf = Array(repeating: CChar(0), count: str.count + 1)

    // copy C string in str into buf
    (str as NSString).getCString(
      &buf, maxLength: buf.count,
      encoding: NSUTF8StringEncoding)

    // save the array object
    self.cString = buf

    // save the pointer
    self.cString.withUnsafeMutableBufferPointer { ptr in
      self.address = ptr.baseAddress
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
    cstr.append(cs.address)
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
