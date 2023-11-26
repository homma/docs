----
title: sclang の言語仕様
status: draft
author: homma
----

## プログラミング言語仕様
- https://cs.wellesley.edu/~cs203/lecture_materials/sclang_basics/sclang_basics.pdf

### グローバル変数

1 文字のアルファベットをグローバル変数として使用できます

````
> a = "foo"
````

ただし、`s` はすでに使用されているため、上書きしない方が良いとされています  

### 環境変数

チルダで始まる文字列を環境変数として使用できます  
環境変数はグローバル変数のように使用することができます

````
> ~foo = "foo"
````

### 擬似オブジェクト

丸括弧で擬似オブジェクトを作成できます  
実態は `Event` オプジェクトです

````
> v = ()
> v.foo = "foo"
> v.foo
````

````
> v = ()
> v.fn = { postln("foo") }
> v.fn()
````

````
> v = ()
> v.f2 = { | a | a.postln }
> v.at('f2').value(10)
````

### スコープ

丸括弧でスコープを作成することができます

````
(
  var foo = "foo";
  foo;
)
````

### ローカル変数

作成されたスコープの中で `var` を使用してローカル変数を作成できます

````
(
  var foo = "foo";
  foo
)
````

