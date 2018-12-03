## About

- [The Definition of Standard ML (Revised)](http://sml-family.org/sml97-defn.pdf) を読んで、要点をまとめます

### 仕様書のページ数
- 本文が 120 ページほどなので、分量はそれほど多くない
- [Haskell Report 1.0](https://www.haskell.org/definition/haskell-report-1.0.ps.gz) は 133 ページ
- [Standard ECMA-262](https://www.ecma-international.org/publications/files/ECMA-ST-ARCH/ECMA-262,%201st%20edition,%20June%201997.pdf) が 95 ページ
- [Revised6 Report on the Algorithmic Language Scheme](http://www.r6rs.org/final/r6rs.pdf) は 90 ページ（ただし分冊されており、他にライブラリのドキュメントなどがある）

- [Haskell 2010 Language Report](https://www.haskell.org/definition/haskell2010.pdf) は 309 ページ
- [ECMAScript® 2018 Language Specification](https://www.ecma-international.org/publications/files/ECMA-ST/Ecma-262.pdf) は 801 ページ

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

識別子。

#### 識別子の種類

````
vid : VId : value identifiers
long vid

tyvar : TyVar : type variables

EtyVar : equality type variables

TyCon : type constructors
long TyCon

Lab : record labels

strid : StrId : structure identifiers
long strid
````

#### qualified identifiers

`long` が付いている識別子は、`structure identifier` で修飾可能なもの。  

`structure identifier` と `identifier` はドットで接続する。

````
strid.id
````

structure identifier で修飾された identifier を `qualified identifiers` と呼ぶ。

ドキュメントの中でただ `identifier` と記載されているものは、`non-qualified identifier`。

#### 識別子の形式

- アルファベットか prime (`'`) かシンボル文字で開始される必要がある
- 使用可能な文字は下記の通り
- ただし、予約語と同じ文字は使用できない
- ただし、`=` は予約語であるが識別子として使用可能

使用可能な文字:
- アルファベット
- 数字
- prime : `'`
- 下線 : `_`
- シンボル文字

シンボル文字:
````
!
%
&
$
#
+
-
/
:
<
=
>
?
@
\
~
`
^
|
*
````

#### 型変数の識別子の形式

`'` で始まる識別子は、`tyvar` もしくは `EtyVar`。

`tyvar` は、`'` の後にアルファベットもしくは数字。  
`EtyVar` は `'` が 2 つ以上続いた後にアルファベットまたは数字。

#### 型変数以外の識別子の形式

`'` で始まらない識別子には、`vid`、`TyCon`、`Lab`、`strid` がある。

`vid` は `'` で始まらない。  
識別子が、`TyCon`、`Lab`、`strid` 以外であった場合は、`vid`。

`TyCon` は `'` で始まらない。  
`*` を含まない（タプルと混同しないように）。
識別子が、型の部分に出現した場合は `TyCon`。

`Lab` は `'` で始まらない。  
0 以外の数字を指定可能（タプルの参照用）。  
識別子が、レコード型の先頭、レコードパターン、レコード表現の中に出現した場合は `Lab`。

`strid` は `'` で始まらない。  
識別子が、`.` の前、もしくは `open` 宣言の中に出現した場合は `strid`。

### 2.5 Lexical analysis

字句解析で出現するアイテムは以下。

- a reserved word  // 予約語
- a numeric label  // これは何?
- a special constant  // リテラル
- a long identifier  // なんで long だけ?
- comment  // コメント (* .. *)
- formatting characters  // フォーマット文字列 /f..f/

それぞれ最長マッチで識別される。

### 2.6 Infixed operators

#### fixity ディレクティブによる識別子の中置演算子化

`infix`、`infixr`、`nonfix` は `fixity` ディレクティブ。

以下の宣言で識別子を中置演算子に設定できる。

````sml
infix d vid1 ... vidn
infixr d vid1 ... vidn
nonfix vid1 ... vidn

d : 演算子の優先順位（十進数）
数値が高いほど優先順位が高い。
オプションのため、設定しなくても良い。
デフォルトの優先順位は 0。

infix : 左結合
infixr : 右結合
````

同じ優先順位の中置演算子に付いて、右結合と左結合のものを並べて使うことはできない (illegal)。

`qualified identifier` は中置にできない。

中置演算子化の設定は `nonfix` ディレクティブで打ち消せる。

中置演算子の他の表現やパターンに対する優先順位は Apendix B 参照。

#### op による中置演算子の前置化

以下の宣言で中置演算子を前置できる。

````sml
opvid1
````

`op` は中置演算子を中置しない場合にのみ有効。

#### `fixity` ディレクティブの有効範囲

以下の `dec` の部分で `fixity` ディレクティブが使用された場合は、中置演算子の有効範囲は `end` まで。

````
let dec in ... end
local dec in ... end
````

それ以外の場合は、`fixity` ディレクティブが使用された以降のプログラム全体が有効範囲。

`fixity` ディレクティブと `op` は parsing が終わった後は使用されない。

### 2.7 Derived Forms

`bare language` と `derived form` がある。  
`derived form` は `bare language` に変換可能。  
`derived form` については Apendix A を参照。

以下はすべて `deribed form`。  
タプルはレコードの変形であることが分かる。

- タプル : `(a, b, c)` => `{1=a, 2=b, 3=c}`
- レコード参照 : `#foo` => `fn {foo=vid ...} => vid`
- `if ... then ... else ...` => `case ... of true => ... | false => ...`
- リスト : `[a, b, c]` => `a :: b :: c`
- 関数 : `fun ... ...` => `val ... rec ...`

### 2.8 Grammar

#### Core 言語のフレーズクラス

````
atexp : atomic expressions
exprow : expression rows
exp : expressions
match : matches
mrule : match rules

dec : declarations
valbind : value bindings
typbind : type bindings
datbind : datatype bindings
conbind : constructor binding
exbind : exception bindings

atpat : atomic patterns
patrow : pattern rows
pat : patterns

ty : type expressions
tyrow : type-expression rows
````

#### 文法表現規則

- optional
````
<>
````

- syntax class
````
xseq ::= x (singleton sequence)
           (empty sequence)
         (x1, ... ,xn) (sequence, n >= 1)
````

- alternative form precedence
  - 後に記載されているほど優先順位が低い

- associative
````
L : left associative
R : right associative
````

- type bind は expression より優先順位が高い

- match の中で match を使う場合は、括弧でくくる必要がある場合がある

#### Grammar: Patterns and Type expressions

````
atpat  ::= _                       (wildcard)
           scon                    (special constant)
           <op>longvid             (value identifier)
           { <patrow> }            (record)
           ( pat )

patrow ::= ...                     (wildcard)
           lab = pat < , patrow>   (pattern row)

pat    ::= atpat                   (atomic)
           <op>longvid atpat       (constructed pattern)
           pat1 vid pat2           (infixed value construction)
           pat : ty                (typed)
           <op>vid<: ty> as pat    (layered)

ty     ::= tyvar                   (type variable)
           { <tyrow> }             (record type expression)
           tyseq longtycon         (type construction)
           ty -> ty'               (function type expression (R))
           ( ty )

tyrow  ::= lab : ty < , tyrow>     (type-expression row)
````

### 2.9 Syntactic Restrictions


#### Grammar: Expressions, Matches, Declarations and Bindings


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