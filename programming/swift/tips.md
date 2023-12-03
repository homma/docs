---
title: Swift Tips
status: draft
author: homma
---

#### Swift の String を C 文字列に変換する

Swift の文字列は C の関数に渡される際に自動的に C の文字列への変換が行われます  
`.utf8` や `.utf8CString` を使用して明示的に変換することも可能です  

````swift
import Darwin

// String
let s = "foo"

// 自動で変換される
puts(s)

// [UInt8]
let sa = Array(s.utf8)
puts(sa)

// [Int8], [CChar]
let sa2 = Array(s.utf8CString)
puts(sa2)
````

- https://developer.apple.com/documentation/swift/string/utf8view
- https://developer.apple.com/documentation/swift/string/utf8
- https://developer.apple.com/documentation/swift/string/utf8cstring

#### Swift 名前空間

class の中などで Swift の既存の関数と名前の重複が発生してしまった場合は、`Swift.` を付けることで Swift 組み込みの関数を呼び出せる

````swift
Swift.print()
````

#### print() で改行させない
- https://developer.apple.com/documentation/swift/print(_:separator:terminator:)

````swift
print("foo", terminator: "")
````

#### 値を返す if

- https://docs.swift.org/swift-book/documentation/the-swift-programming-language/controlflow/#If

Swift の `if` は通常は statement  
ただし、`=` の右辺に `if` を書くと `if expression` になり、値を返すことができるようになる

````swift
let s = if true { "it's true." } else { "no." }
````

`if expression` の場合は `else` 節が必須  

`return` 文でも使用可能

````swift
func foo() -> String { return if true { "TRUE" } else { "FALSE" } }
````

クロージャでも使える

````swift
({ if true { "T" } else { "F" } })()
````

#### 値を返す switch

- https://docs.swift.org/swift-book/documentation/the-swift-programming-language/controlflow/#Switch

Swift の `switch` は `=` の右辺に書くと値を返すことができるようになる  
`if` と同様に、expression になる  

JavaScript でも採用してほしい  
JavaScript ではパターンマッチ `match/when` が提案されており、`match` は値を返せるみたい

- https://github.com/tc39/proposal-pattern-matching

#### if case let
- https://docs.swift.org/swift-book/documentation/the-swift-programming-language/patterns/

`for case let` もある

#### Buffer

C 言語とバッファ経由でデータのやり取りをしたい場合は以下のように書ける

````swift
var buf = Array(repeating: UInt8(0), count: 128)
````

ポインターで渡す必要がある場合は `let` ではなく `var` を使用する  
`&buf` でポインターを作って渡すことができる

#### OpaquePointer

- https://developer.apple.com/documentation/swift/opaquepointer

opaque pointer の操作には `OpaquePointer` 型が用意されている  
ポインターが指すデータの構造は知らなくて良いという特徴をそのまま表現できる  

#### ビットパターンを維持したキャスト

- https://developer.apple.com/documentation/swift/int/init(bitpattern:)-72037

bitPattern オプションを使用することで、値ではなくビットパターンを使用してキャストすることができる  

````swift
let uval: UInt16 = 65535
let sval = Int16(uval) // Error
let sval = Int16(bitPattern: uval) // OK
````

明示的に動作を指定できるため、値を維持してキャストされるのか、ビットの並びを維持してキャストされるのかを考えなくて良い

