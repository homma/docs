
ローカルファイルの全文検索について

# 結論 as of 2023/1/5
- ripgrep を試す
- ripgrep が自分のユースケースを満たす場合は、ripgrep を使う
- ripgrep が要件を満たしていない場合は、Code Search について調査する

# 動機

現在、以下のように実行しているローカルファイルのテキスト検索を高速化・効率化したい

````
% find . | while read i; do grep -sHi <Search Keyword> ${i}; done
````

# 検討したこと

## Spotlight and mdfind
- Spotlight はオフにしているので使えない
- デーモン型ではなく、任意にプロセスを実行したい
- インデックスの作成は手動で必要なタイミングに実行したい
- 実装がわからないため、挙動も把握できない

## Apache Lucene
- デファクト的な位置付けで信頼できる
- 開発終了になる可能性はほとんどなく、安心して使える
- 日本語にも対応しているみたい
- ソフトウェアの性質的に、そのままでは全文検索ツールとして使用できない
- 情報もそれほどない
- 実行に Java VM が必要
- インデックスの作成自体はコマンドで実行可能
- https://lucene.apache.org/core/9_4_2/demo/index.html
- https://lucene.apache.org/core/9_4_2/queryparser/org/apache/lucene/queryparser/flexible/standard/StandardQueryParser.html
- Kotlin でプログラムを書いて使うのはあり
- Apache Lucene を使いこなせたら楽しそう

## Tantivy
- https://github.com/quickwit-oss/tantivy
- Rust 製
- 日本語の検索もできるっぽい
- CLI も用意されているほか、Python からも使用可能
- 問題なさそう

## Lunr.js
- JavaScript
- https://github.com/olivernn/lunr.js
- 開発が止まっている
- 日本語の検索も可能とのこと
- https://blog.kozakana.net/2019/03/lunr-node/
- https://zenn.dev/retrorocket/articles/d118b3ea6870f4

## Code Search
- https://github.com/google/codesearch
- https://swtch.com/~rsc/regexp/regexp4.html
- Trigram でインデックスを作成
- Go 製
- ripgrep が自分の用途に合わなかったら試す
- 実装を調べるのが楽しそう

## Hound
- https://github.com/hound-search/hound
- Go 製
- Code Search をベースにしている

## CodeSearch
- https://medium.com/@colin353/code-search-74a6a0a74789
- https://github.com/colin353/universe/tree/master/tools/search
- ↑↑の Code Search とは別物
- Rust 製
- これも Trigram を使用
- VIM からも使用可能

## Bloom Filter を使用した全文検索エンジン
- https://www.stavros.io/posts/bloom-filter-search-engine/
- これは空白区切りのため、別途形態素解析ツールが必要と思われる

## ripgrep
- https://github.com/BurntSushi/ripgrep
- grep を並列実行するため、マルチコア・スレッド環境では高速
- それ以外にも高速化されている
- 全文検索エンジンの使用例が少ないのは、この系統の grep で間に合っているからなのかもしれない
- 情報もたくさんある
- インストールも簡単

````
$ brew install ripgrep
````

- grep なのでインデックスは作成しないが、高速とのこと
- インデックスを作成しなくても高速なのであれば、使いやすそう
- そのくらい高速かは使ってみないとわからない

- インデックスを作成する全文検索エンジンは、インデックス作成の手間と時間が必要
- インデックスを作成するタイプのものはインデックスのサイズも気になる

## その他
- https://gitlab.gnome.org/GNOME/tracker
- https://github.com/weixsong/elasticlunr.js
- https://github.com/fergiemcdowall/search-index
- https://github.com/tinysearch/tinysearch
- https://github.blog/2021-12-15-a-brief-history-of-code-search-at-github/

- RDBMS 系はユースケースに合わないため、検討せず

