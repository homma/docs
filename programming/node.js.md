Node.js 固有の情報をまとめます

# インストール

## nodebrew を使用したインストール
- インストール手順
````sh
$ brew install nodebrew
$ nodebrew setup_dirs
$ nodebrew install-binary latest
$ nodebrew use latest
$ export PATH=${HOME}/.nodebrew/current/bin:${PATH}
$ node -v
v8.5.0
$ npm -v
5.3.0
````

- nodebrew setup_dirs すると、~/.nodebrew ディレクトリが作成される
- node も npm も ~/.nodebrew/node/{version} 以下にインストールされる
- nodebrew use すると、~/.nodebrew/current にシンボリックリンクが作成される
- バイナリは nodejs.org から取得されている感じ

# 使い方

## WebAssembly
````javascript
> WebAssembly
{}
````

## util
- https://nodejs.org/api/util.html

## Node.js で JSON をパースする
````javascript
$ JSON='{"foo": "foo", "bar": "bar"}'
$ echo ${JSON} | \
node -e "process.stdin.on('data', d => console.log(JSON.parse(d).foo))"
foo
````

- 複数行でも OK
````javascript
$ JSON='{
"foo": "foo",
"bar": "bar",
"baz": "baz"
}'
$ echo ${JSON} | \
node -e "process.stdin.on('data', d => console.log(JSON.parse(d).bar))"
bar
````

- デフォルトでは YAML は扱えない（パッケージのインストールが必要）

## Node.js で標準入力を扱う
- stdin はバイナリなので、stdout.write で書き出す、toString() で文字列に直す、setEncoding でエンコードするなどが必要
````sh
$ echo "foo" | node -e "process.stdin.on('data', d => process.stdout.write(d))"
foo
$ echo "foo" | node -e "process.stdin.on('data', d => console.log(d.toString()))"
foo

$ echo "foo" | \
node -e "process.stdin.setEncoding('utf-8').on('data', d => console.log(d))"
foo

````

# ツール情報

## Node.js のバージョンアップ

バイナリインストールは install-binary サブコマンドを使用
````sh
$ nodebrew ls
$ nodebrew ls-remote
$ nodebrew install-binary latest
$ nodebrew use <version>
$ nodebrew ls
$ node -v
$ nodebrew uninstall <old-version>
````

## npm のバージョンアップ
Nodebrew でインストールした場合もこの方法で OK

````
$ npm i -g npm
````

i は install  
ちゃんと ~/.nodebrew 配下の npm が更新されました  

## Nodebrew で Node.js をソースからインストールする
- 最新の Node.js をインストールしようとして、間違ってソースからビルドしてしまったので記録

ソースコードからインストールする場合は install サブコマンドを使用
````sh
$ nodebrew install latest
````

- ソースコードからビルドすると、だいぶ時間がかかる（間違ってやるとショックが大きい）
- ~/.nodebrew/src/&lt;version> 以下に数 GB のファイルが作成される (v9.3.0 では 3.5GB でした）
- ~/.nodebrew/src 以下のファイルは clean サブコマンドで削除可能

````sh
$ nodebrew ls
$ nodebrew ls-remote
$ nodebrew install latest
...
$ nodebrew clean <version>
````

## Node.js のインストールおよびバージョン管理方法

Node.js のインストールと複数バージョンの管理には色々なツールが存在している

- [nodenv](https://github.com/nodenv/nodenv)
- [NVM](https://github.com/creationix/nvm)
- [nodebrew](https://github.com/hokaccha/nodebrew)
- [ndenv](https://github.com/riywo/ndenv)
  - [anyenv](https://github.com/riywo/anyenv)
  - [node-build](https://github.com/riywo/node-build)

- nodebrew
  - homebrew でインストール可
  - Perl で書かれている
  - Linux でも使用可能っぽい
  - global install されたパッケージを migrate できる
  - 日本人作（おそらくユーザーも日本人が多い）
  - Node.js のバイナリインストール可能
  - Node.js の最新バージョンにも追随できる（curl でバージョンを問い合わせているみたい）
- ndenv
  - homebrew でインストール可
  - シェルスクリプトで実装されている
  - rbenv の fork
  - ディレクトリごとに使用する Node.js のバージョンを変更できる（複数プロジェクトでバージョンを使い分けられる）
  - anyenv は homebrew でインストールできない
  - 日本人作（おそらくユーザーも日本人が多い）
- NVM
  - homebrew でインストール可
  - bash で書かれている
  - .bash_profile からシェルの関数を読み込む方式（nvm というコマンドがあるわけではない）
  - シェルの起動が遅くなるという報告あり
  - zsh との相性が悪い or 悪かったらしい（詳細不明）
  - macOS 以外でも使用可能
  - Node.js のバイナリインストール可能
- homebrew
  - インストールできるのは最新のバージョンのみ（複数のバージョンの切り替えができない）
  - Node.js の最新バージョンは 8.5.0 ですが、本日時点では Homebrew でインストール可能なのは 8.4.0 でした
  - Node.js の開発速度に追随できていないのかも

- 以下の理由で nodebrew を使うことにしました
  - homebrew は最新バージョンに追随できていない
  - NVM はシェルの起動が遅くなる可能性がある
  - 最新バージョンが使えればよく、複数プロジェクトでバージョンを切り替える必要はない
  - Perl で実装されているものの、何か問題があったら自分で調査できそう

# その他

## Glitch
- https://glitch.com
- Node.js アプリの無料ホスティングサービス
- データはローカルファイルに保存できる
- https://glitch.com/faq#db
