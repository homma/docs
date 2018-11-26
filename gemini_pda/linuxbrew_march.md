
## superenv がコンパイラフラグに -march=native を追加する問題の対応

Linuxbrew でソフトウェアをインストールする際に、以下のようなエラーが発生することがあります。

````
configure: error: in `/tmp/libatomic_ops-20181125-13175-fxemgx/libatomic_ops-7.6.6':
configure: error: C compiler cannot create executables
See `config.log' for more details
````

`config.log` ファイルを確認すると、`-march=native` オプションの問題であることがわかります。

````
$ less ~/.cache/Homebrew/Logs/libatomic_ops/config.log  
...
conftest.c:1:0: error: unknown value 'native' for -march
 /* confdefs.h */
 ^
Assembler messages:
Error: unknown architecture `native'

Error: unrecognized option -march=native
````

しかしながら、Formula には `-march=native` の指定はありません。

````sh
$ brew cat libatomic_ops | grep configure
    system "./configure", "--disable-dependency-tracking", "--prefix=#{prefix}"
````

Linuxbrew (Homebrew) には、`superenv` という仕組みがあり、適切と思われる値で環境変数の書き換えを行っています。
上記の場合は、`-march=native` の追加が行われています。

しかしながら、ARM 版の GCC の `-march=native` のサポートは GCC 8 以降からであるため、エラーが発生してしまっています。

ad-hoc な対応ですが、`superenv.rb` の "-march=native" を "" に変更することで問題を回避することができます。

````sh
$ sudo vi /home/linuxbrew/.linuxbrew/Homebrew/Library/Homebrew/extend/ENV/super.rb
// "-march=native" を "" に変更する
````

変更内容は以下の通りです。

````diff
$ git diff
diff --git a/Library/Homebrew/extend/ENV/super.rb b/Library/Homebrew/extend/ENV/super.rb
index 2b5951fef..a2e03093a 100644
--- a/Library/Homebrew/extend/ENV/super.rb
+++ b/Library/Homebrew/extend/ENV/super.rb
@@ -236,7 +236,7 @@ module Superenv
     elsif Hardware::CPU.intel? && !Hardware::CPU.sse4?
       Hardware::CPU.optimization_flags.fetch(Hardware.oldest_cpu)
     elsif ![:gcc_4_0, :gcc_4_2].include?(compiler)
-      "-march=native"
+      ""
     # This is mutated elsewhere, so return an empty string in this case
     else
       ""
````

この状態で改めて `brew install` を実行することで、ソフトウェアのインストールを完了させることができます。

