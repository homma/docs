
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