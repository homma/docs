# 旅人とドラゴン

## 緑色の目をした竜（問題編）
[Green-eyed dragons](https://www.physics.harvard.edu/uploads/files/undergrad/probweek/prob2.pdf) から。

あなたは遠く離れた無人の島にたどり着きました。  
そこにはとても友好的な 100 匹の竜が住んでいて、どの竜の目も緑色でした。  
竜たちは何百年ものあいだ人間と交流がなく、あなたの到着にとても興奮しました。  
彼らはあなたを島の隅々まで案内し、竜の暮らしの全てを教えました（もちろん竜は人間の言葉を話せます）。  

竜たちはとても普通に暮らしているように見えましたが、ある時、とても奇妙なことに気がつきました。  
彼らには一つの規則があって、それは、竜は自分の目の色が緑色だと見つけた場合は、発見したその日のちょうど真夜中に、竜の持つ全ての力を捨てて、尾羽の長い雀にならないといけないというものでした。  
ただ、島には鏡がありませんでしたし、竜たちは目の色についての話は絶対にしませんでしたので、長い間、何も知らずに幸せに暮らしてきました。  

あなたが島を離れる時、全ての竜が集まってあなたを見送りにきました。  
あなたは涙ながらに、彼らの暖かいもてなしへの感謝を伝えました。  
そして、竜たちに彼らが既に知っていることを教えました（竜は他の竜の目の色は見知っています）。  
少なくとも 1 匹の竜は緑色の目をしていることを。  
そしてあなたは島を離れました。  
これから（何かが）起きるかについて考えることもなく。  
竜たちは（もちろん）間違いなく論理的な思考をしていると考えると、これからどんなことが起きるでしょうか?  

もし何か興味深いことが起きるのであれば、あなたは竜たちにどんな追加の情報を与えるでしょうか?  

### 用語
- desert island : 無人島
- inhabited : 居住する
- as far as dragons go : 竜に関していえば
- blissful : 幸せの
- infallibly : 誤りをおこさない

## 緑色の目をした竜（解決編）
- [Green-eyed dragons](https://www.physics.harvard.edu/uploads/files/undergrad/probweek/sol2.pdf) から。

まずは竜の数が少ない場合で考えてみましょう。  
100 匹の代わりに、竜の数を N と表現します。  
その方が問題を解いている感じがしますので。  

N = 1 の場合、あなたがこの竜に対して、少なくとも 1 匹の竜が緑色の目であると伝えたということは、伝えた相手の竜が緑色の目をしていることに他ありません。  
そのため、彼は真夜中になったら雀にならなければなりません。  

N = 2 の場合、それぞれの竜を A と B と呼ぶことにします。  
少なくとも 1 匹の竜が緑色の目であると伝えたら、A は自分自身についてこう考えます。  

「もし自分が緑色の目でなかったら、B は自分 (A) が緑色の目ではないと見てわかるはず。  
その場合、B は彼女自身が緑色の目に違いないと結論づけるだろう。  
その結果、彼女は最初の日の真夜中に雀にならないといけない」  

したがって、もし B が最初の日の真夜中に雀にならなかったら、A は自分が緑色の目であったと結論付け、二日目の真夜中に雀になります。  

B も同じような思考過程を辿りますので、結果として 2 匹とも二日目の真夜中に雀になります。  

N = 3 の場合、竜を A, B, C と呼ぶことにします。  
あなたが情報を伝えたあと、C は自分自身についてこう考えます。  

「もし自分が緑色の目でなかったら、A と B は C が緑色の目ではないと見てわかるはず。  
その場合、彼らは N = 2 の場合の理屈を使って A と B が緑色の目であるかどうかを調べることができる。  
その結果、彼らは二日目の真夜中に雀になる」  

したがって、もし A と B が二日目の真夜中に雀にならなかった場合、三日目に C は自分自身が緑色の目であると結論付け、三日目の真夜中に雀になります。  
A と B も同じような思考過程を辿りますので、結果として全ての竜が三日目の真夜中に雀になります。  
これで法則が明らかになりました。  

[主張]  
N 匹の竜がいて、全ての竜が緑色の目をしていた場合。  
もしあなたが彼らに、少なくとも 1 匹の竜は緑色の目をしていると伝えたら、彼らは N 日目の真夜中に全て雀になる。  

[証明]  
帰納法を使って証明します。  
竜の数が N 匹の場合に上記の主張が真になると仮定し、竜の数が N+1 の場合にも真になることを明らかにします。  
すでに N = 1,2,3 の場合については結果が出ています。  

N+1 匹の竜がいた場合、そのうちの 1 匹を A と呼ぶことにします。  
あなたが情報を伝えたら、彼女は自分自身についてこう考えます。  

「もし自分が緑色の目でなかったら、他の N 匹の竜は A が緑色の目でないと見てわかるはず。  
その場合、彼らは N 匹の場合の理屈を使って、自分たちが緑色の目であるかどうかを調べることができる。  
その結果、彼らは N 日目の真夜中に雀になる」  

したがって、もし他の竜が N 日目の真夜中に雀にならなかった場合、N+1 日目に A は自分自身が緑色の目であると結論付け、N+1 日目の真夜中に雀になります。  
他の N 匹の竜も同じような思考過程を辿りますので、彼らも N+1 日目の真夜中に雀になります。  

結果として、この問題では、100 匹の竜全てが 100 日目の真夜中に燕になります。  

これで問題は解けましたが、一見意味がないような情報が重大な結果をもたらしたことに困惑しているかもしれません。  
あなたの言ったことを全ての竜が確実に事前に知っていたのであれば、どうしてこのようなことが起きたのでしょうか。  
あなたは竜たちに何か新しい情報をもたらしたのでしょうか?  
その答えは `Yes` です。  
何が新しい情報だったのか見てみましょう。  

N = 1 の場合を考えてみます。  
この場合、新しい情報がなんであったのかは明確です。  
あなたは 1 匹しかいない竜に対して、彼が緑色の目の竜だと伝えました。  
しかし、N >= 2 の場合は、新しい情報がなんであったのか、より不明瞭になります。  

N = 2 の場合を考えてみます。  
あなたが情報を伝える前、A は B が緑色の目であることを知っていましたし、B も A が緑色の目であることを知っていました。  
それが彼らの知りうる全てであり、そこからなんらかの結論を導き出すことはできません。  
しかし、あなたが少なくとも 1 匹の竜が緑色の目だと伝えたことで、A の持つ情報は 2 つになりました。  
1. 彼は B が緑色の目であると知っています  
2. 少なくとも 1 匹の竜が緑色の目であることを B が知ったことを知りました（A は B があなたの情報を聞いたことを知っていますので）  
B も同じように 2 番目の情報を獲得しました。  
すでに検証した N=2 の場合の通り、この 2 番目の情報が重大でした。  

N = 3 の場合についても考えてみます。  
A は B が緑色の目であると知っています。  
そして A は、B が少なくとも 1 匹の竜が緑色の目であることを知っていることを知っています（なぜなら、B が C の目が緑色であることを見たことを A は知っていますので）。  
ということは、N = 2 の場合の情報は、あなたが伝える前にすでに知られていたことになります。  
それでは、あなたの言葉により、どんな新しい情報が伝わったのでしょうか?  
あなたが話した後で正しいとわかったことは、少なくとも 1 匹の竜が緑色の目であることを C が知っていることを B が知っていると A が知ったということです。  

同じ結論は一般化された数 N に対しても有効です。  
パラドックスがあったりはしません。  
あなたの言葉から情報がもたらされていたのです。  
あなたが伝えた情報以上の情報が竜たち全体にもたらされました。  
そしてその結果、主張の照明で見たように、もたらされた情報は、竜たち全員が最終的に彼らの目の色を知るのに十分なものでした。  

まとめると、あなたが情報を伝える前は、N 匹の竜に対し、以下の論述が当てはまりました。  

- 少なくとも 1 匹の竜が緑色の目であることを An-1 が知っていることを An-2 が知っていることを ... A3 が知っていることを A2 が知っていることを A1 は知っている  

なぜなら、An-1 は An の目を見ていますし、An-2 は An-1 が An の目を見ていることを見ていますし、他の竜も同じようにして、最終的には A1 は A2 が、An-1 が An の目を見ていることを ... 見ていることを、見ています。  
同じ結論はどんな N-1 のグループに対しても有効です。  
要点としては、あなたが情報を伝えたことで初めて N 番目の竜まで連鎖が繋がったということです。  
N 番目の竜があなたの情報を聞いたということは、この連鎖にとって決定的な意味があります。  

結果として、この連鎖「A は B 以降の竜が情報を知っていることを知っており、B は C 以降の竜が情報を知っていることを知っており、、、」がどこまで続いているのかが非常に重要な意味を持ちます。  

もし竜が 1 匹でもあなたの情報（少なくとも 1 匹の竜が緑色の目をしていること）を聞き逃していたら、その場合は、竜たちは引き続きずっと幸せな竜のままでいるでしょう。  
### 用語
- seemingly : 外見上は