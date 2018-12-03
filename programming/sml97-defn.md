
## Preface

### B |- P => M

````
B |- P => M

B : Background
variable declarations, procedure declarations, etc.

P : Phrase
phrase of the programming language.

M : Meaning
results from evaluating any phrase P.
````

B (変数定義、関数定義など) の状況のもとに、P (コード) を評価した結果、得られる意味が M

## 1. Introduction

### interactive language
- a sequence of top-level declaration
- top-level environment (basis)

### phase in execution of a declaration
- parsing
- elaboration
- evaluation

#### elaboration
- static phase
- check well-typed?
- check well-formsed?
- records type information
- records form information

#### evaluation
- dynamic phase

### formal definition
- grammatical rules (for parsing)
- elaboration rules (for elaboration)
- evaluation rules (for evaluation)

### basis
- static basis
- dynamic basis

### structure of the language
- Core language (Core)
- Modules
- Programs

### sections of the document
- 2 : Core - Syntax
- 3 : Modules - Syntax
- 4 : Core - Static Semantics
- 5 : Modules - Static Semantics
- 6 : Core - Dynamic Semantics
- 7 : Modules - Dynamic Semantics
- 8 : Programs - Syntax, Static Semantics, Dynamic Semantics

### group of phrase classes
- Bare language
- derived forms

### Apendix
- A : the derived forms
- B : a full grammar
- C and D : detailed static and dynamic basis

### form of Natural Semantics

````
A |- phrase => A'
(*
 * against the background provided by A, the phrase 'phrase' elaborates
 * or evaluates to the object A'
 *)

A : static or dynamic basis, etc.

A' : semantic object
type in the static semantics
value in the dynamic semantics
etc.
````

## 2. Syntax of the Core

### 2.1 Reserved Words

Standard ML の予約語。

`=` は identifier として使用可能。  
`=` 以外は identifier として使用できない。

````sml
abstype
and
andalso
as
case
datatype
do
else
end
exception
fn
fun
handle
if
in
infix
infixr
let local
nonfix
of
op
open
orelse
raise
rec
then
type
val
with
withtype
while
(
)
[
]
{
}
,
:
;
...
_
|
=
=>
->
#
````

### 2.2 Special constants

以降、`scon` は、Special constants (`SCon`) のこと。

定数 (`constants`) となっていますがリテラルのことと考えて良さそう。

#### 十進数

````
10
````

#### 負の数

数字の前に `~` をつけると負の数。

````
~10
````

#### 十六進数

````
0xA
~0xF
````

#### word 型の十進数

````
0w10
````

#### word 型の十六進数

````
0wxA
````

#### 実数

````
0.5
2.0E3
````

#### アルファベット

`0 - 255`

#### 文字列

`"` で囲った印字可能文字 (`33-126`)、空白、エスケープシークエンス。

````
"foo"
````

#### エスケープシークエンス

`\` で開始。

````
\a : ASCII 7
\b : backspace (ASCII 8)
\t : horizontal tab (ASCII 9)
\n : linefeed / newline (ASCII 10)
\v : vertical tab (ASCII 11)
\f : form feed (ASCII 12)
\r : carriage return (ASCII 13)
\^c : control character => c (64 - 95) - 64
\ddd : a single character denoted with three digit
\uxxxx : a single character denoted with four hexadecimal digit
\" : "
\\ : \
\f..f\ : ignored / f => formatting characters
````

#### formatting characters

空白、タブ、改行、formfeed を含む非表示文字のサブセット。  
内容は無視される。  

長い行を複数行に分割したい際に使用できる。

````
a long line \
\ continued line
````

#### 文字定数

`#` とサイズが 1 の文字列を連続して書くと文字定数になる。  
文字リテラルと考えて問題なさそう。

````
#a
````

#### 数値型についての補足

複数の数値型を実装することが可能。

#### 文字列型についての補足

複数の文字列型を定義可能。  

ただし、範囲は `0-255`。  
`0-127` は ASCII である必要がある。  
同じ文字列は同じ文字で表現される必要がある（何を想定しているのか不明）。

UTF-8 は Standard-ML の範囲では表現できない。

### 2.3 Comments

コメントは `(*` と `*)` で括る。  
コメントはネスト可能。  
`(*` が `*)` で閉じていない場合はコンパイラにチェックされる。

````
(* this is comment *)
````

### 2.4 Identifiers


### 2.5 Lexical analysis


### 2.6 Infixed operators


### 2.7 Derived Forms

`bare language` と `derived form` がある。  
`derived form` は `bare language` に変換可能。  
`derived form` については Apendix A を参照。

以下はすべて `deribed form`。

- タプル : `(a, b, c)` => `{1=a, 2=b, 3=c}`
- レコード参照 : `#foo` => `fn {foo=vid ...} => vid`
- `if ... then ... else ...` => `case ... of true => ... | false => ...`
- リスト : `[a, b, c]` => `a :: b :: c`
- 関数 : `fun ... ...` => `val ... rec ...`

### 2.8 Grammar


### 2.9 Syntactic Restrictions


## 3 Syntax of Modules

### 3.1 Reserved Words

モジュールの予約語。

````
eqtype
functor
include
sharing
sig
signature
struct
structure
where
:>
````