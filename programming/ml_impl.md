# Implementations of ML-like Languages

## References
- http://sml-family.org/sml97-defn.pdf
- http://sml-family.org
- http://mosml.org/mosmlref.pdf

## Mini ML

List of small/mini/micro/tiny ML language implementations

## Micro ML

### Micro ML
- 実装言語 : SML
- https://github.com/pascallouisperez/mu-ml
> Micro ML — Small, simple compiler in ML for a subset of ML. Meant for teaching.

- lex を使っている
- パターンマッチなし

### microML
- 実装言語 : Haskell
- https://github.com/kellino/microML
> a simple functional language for learners

### Micro ML
- 実装言語 : C++
- https://bitbucket.org/microml/micro-ml/src
> a compiled, functional language with similar constructs as Standard ML, which can be run on a Teensy microcontroller.

### Micro ML
- 実装言語 : F#
- OCaml-like

- Peter Sestoft さんの講義を書籍にしたもの
- https://www.itu.dk/~sestoft/plc/
- [Programming Language Concepts](https://www.springer.com/jp/book/9783319607887)

- 他の大学でも使用されている
- http://homepage.cs.uiowa.edu/~tinelli/classes/3820/Fall18/notes/chap4-micro-ML.pdf
- http://aktemur.github.io/cs321/lectures/lecture04.pdf

- 筑波大学情報科学類の『プログラム言語論』の講義でも参照されているみたい
- http://www.cs.tsukuba.ac.jp/~kam/lecture/plm2015/5.pdf
- http://www.cs.tsukuba.ac.jp/~kam/lecture/plm2016/5.pdf

### micro-ML
- 実装言語 : Python
- https://github.com/eliben/code-for-blog/tree/master/2018/type-inference
- https://eli.thegreenplace.net/2018/type-inference/

- eval はないっぽい

## Mini ML

### plzoo miniml
- 実装言語 : Ocaml
- Ocaml-like
- https://github.com/andrejbauer/plzoo/tree/master/src/miniml
- http://www.lexicallyscoped.com/2015/06/28/miniml-compiler-to-js.html

### minml
- 実装言語 : SML
- https://github.com/melsman/sml-llvm/blob/master/test/miniml/miniml.sml
> (* A Simple ML-like language ported to Standard ML from OCaml; see
>      http://groups.google.com/group/fa.caml/msg/5aee553df34548e2
> *)

- パーサーはない
- パース済みの AST を LLVM アセンブラにコンパイルしているっぽい

### minml
- 実装言語 : Ocaml
- http://groups.google.com/group/fa.caml/msg/5aee553df34548e2
- camlp4 を使っている

### Mini-ML
- 実装言語 : F#
- https://github.com/pjmlp/Mini-ML
> An expression evaluator done in F#.

### mini-ml
- 実装言語 : OCaml
- https://github.com/Aaylor/mini-ml

### MiniML Compiler
- 実装言語 : C
- https://github.com/ichi-rika/miniml

### Mini ML
- 実装言語 : Haskell
- https://github.com/authchir/mini-ml
> A compiler for a subset of ML

### Mini-ML Compiler
- 実装言語 : SML
- 大学の講義
- http://www.cs.cornell.edu/courses/cs312/2002fa/hw/ps5/ps5.html
- http://www.cs.cornell.edu/courses/cs312/2002fa/hw/ps5/ps5.zip

### Min-Caml
- 実装言語 : OCaml
- https://github.com/esumii/min-caml
- http://esumii.github.io/min-caml/tutorial-mincaml-4.htm

### TamTaML
- 実装言語 : OCaml
- https://github.com/nzyuzin/tamtaml
> An implementation of a small ML-like language

### miniML-Parser
- https://github.com/IvanMalison/miniML-Parser

## Small ML

### MLComp
- 実装言語 : SML(SML/NJ), Prolog(SWI Prolog)
- https://github.com/kentdlee/MLComp
> A Compiler and Type Inference System for a subset of Standard ML called Small.

## Other

### SOSML - Online SML
- 実装言語 : TS
- https://github.com/SOSML/SOSML
> SOSML is an online interpreter for the functional programming language Standard ML, written in TypeScript.

- https://sosml.github.io

- DOM を弄れたりはしないっぽい
- 偶然かもしれませんが、日本語にも対応している
````sml
String.size "あいうえお";
> val it = 5: int;
````

### embedded-ml
- https://github.com/andrewtron3000/embedded-ml

### Typer
- 実装言語 : OCaml
- https://gitlab.com/monnier/typer
- https://gitlab.com/monnier/typer/blob/master/doc/primer.md
- [Typer: ML boosted with type theory and Scheme](http://www.iro.umontreal.ca/~monnier/typer-jfla2019.pdf)
- [Typer an ML sibling inheriting from Lisp and Coq](http://www.nlsde.buaa.edu.cn/__local/F/E6/0C/916233955997A7DB81C37FE703A_DBE3A4B8_68260.pdf)

### LangF
- https://github.com/chinmayjain18/Compiler-Construction-LangF

- https://github.com/Markz2z/CPS-Translate-4SML

### 1ML
- 実装言語 : OCaml
- https://github.com/rossberg/1ml

## Incomplete

### WebML
- https://github.com/KeenS/webml
> WebML is to be a Standard ML (SML '97) Compiler that works on web browsers and generates WebAssembly binaries.

SML を WebAssembly にコンパイルする。  
Rust で実装されており、nom を使っている。

- https://keens.github.io/slide/konpairanoninkaramitaWebAssembly/
- https://keens.github.io/slide/WebAssemblytokonpairatorantaimu/

## Compile to JS

### SMLtoJs
- https://github.com/melsman/mlkit/blob/master/README_SMLTOJS.md

### hamlet/compile-js
- https://github.com/rossberg/hamlet/tree/master/compile-js

HaMLet には SML を JavaScript にコンパイルする機能がある。  
ただし実装は 2013 年のもの。

- https://people.mpi-sws.org/~rossberg/hamlet/
> Added simple JavaScript compiler and runtime as a proof of concept, accessible via the newly added -j mode.

## Standard ML

### SML/NJ
- https://www.smlnj.org
- https://smlnj-gforge.cs.uchicago.edu/scm/viewvc.php/?root=smlnj

### MLton
- http://mlton.org
- https://github.com/MLton/mlton

### PollyML
- https://www.polyml.org/
- https://github.com/polyml/polyml

### MLkit
- http://www.elsman.com/mlkit/
- https://github.com/melsman/mlkit

### Moscow ML
- http://mosml.org
- https://github.com/kfl/mosml

### MLWorks
- https://github.com/Ravenbrook/mlworks
> MLWorks is a Standard ML compiler and development environment.

- https://github.com/Ravenbrook/mlworks/blob/master/license.txt
> The open source license for MLWorks is the BSD 2-Clause License_.

Linux で動作するみたい。
Standard ML で実装されているため bootstrap が必要。

### sml2c
- http://www.nic.funet.fi/pub/languages/ml/sml2c/

### TILT
- https://github.com/RobertHarper/TILT-Compiler

### HaMLet
- https://people.mpi-sws.org/~rossberg/hamlet/
- https://github.com/rossberg/hamlet

### CakeML
- https://cakeml.org
- https://github.com/CakeML/cakeml
- https://github.com/CakeML/cakeml/tree/master/compiler
- https://github.com/CakeML/cakeml/tree/master/compiler/parsing

### Alice ML
- http://www.ps.uni-saarland.de/alice/

### SML#
- http://www.pllab.riec.tohoku.ac.jp/smlsharp/
