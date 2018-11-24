
## superenv がコンパイラフラグに -march=native を追加する問題の対応

### super.rb の編集

````sh
$ sudo vi /home/linuxbrew/.linuxbrew/Homebrew/Library/Homebrew/extend/ENV/super.rb
// "-march=native" を削除する
````

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

`git commit` が必要かも。