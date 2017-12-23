# インストール

rustup を使用することが推奨されています  
- [Rustのインストール](https://www.rust-lang.org/ja-JP/install.html)
- [rustup](https://github.com/rust-lang-nursery/rustup.rs/blob/master/README.md)

rustup をインストールすると、rustc や cargo もインストールされます

````sh
$ curl https://sh.rustup.rs -sSf | sh
$ echo 'export PATH="${HOME}/.cargo/bin:$PATH"' >> ~/.profile
$ rustc --version
````

rustup の削除は rustup self uninstall
````sh
$ rustup self uninstall
````

rust 環境の更新は rust update
````sh
$ rust update
````

rustup の更新は rustup self update
````sh
$ rustup self update
````

# ドキュメント

## 学習リソース
- [関数型プログラマのための Rust](http://postd.cc/rust-for-functional-programmers/)
- [Rust by Example](http://rust-lang-ja.org/rust-by-example/)

# 関連サイト
- [rustup](https://github.com/rust-lang-nursery/rustup.rs)

# ツールの使い方

## Rust のバージョンアップ
rustup update でバージョンアップできます

````sh
$ rustc --version
rustc 1.20.0 (f3d6973f4 2017-08-27)
$ rustup update
...
$ rustc --version
rustc 1.22.1 (05e2e1c41 2017-11-22)
````

# その他

## メソッドの追加
JavaScript でよく書くこれは、
````javascript
function myobj() {
  this._name = "myobj";
}

myobj.prototype.about = () => { console.log("This is my object.") }
myobj.prototype.name = function() { console.log(this._name) }

let m = new myobj();
m.about();
m.name();
````

Rust だとこうなる
````rust
struct MyObj {name: String}

impl MyObj { fn about(&self) { println!("This is my object.") } }
impl MyObj { fn name(&self) { println!("{}", self.name) } }

fn main() { let m = MyObj{name: "myobj".to_string()}; m.about(); m.name(); }
````

JavaScript いいなと思ってしまいますが、Rust もそのうち慣れるでしょう

## ウェブブラウザで実行する
- Emscripten を使用せず、直接 wasm を生成できるようになりました

### Emscripten を使用する方法
Emscripten で Rust のコードから JavaScript のコードを生成できます。  
ただし、Rust のコードがそのまま JavaScript に変換されるわけではありません。  
asm.js については詳しくないので、詳細は要確認。  
DOM をいじるのはハードルが高そう。

JavaScript の置き換えとして使うには十分ではない感じ。

Emscripten のインストール
````sh
$ rustup target add asmjs-unknown-emscripten
$ mkdir -p ~/Applications/Emscripten; cd $_
$ curl -O -# https://s3.amazonaws.com/mozilla-games/emscripten/releases/emsdk-portable.tar.gz
$ tar zxf emsdk-portable.tar.gz
$ cd emsdk-portable
$ ./emsdk update
$ ./emsdk install latest
$ ./emsdk activate latest
$ echo ". Applications/Emscripten/emsdk-portable/emsdk_env.sh" >> ~/.profile
$ . ~/.profile
````

Homebrew でインストールすると、node もインストールされてしまい、node のバイナリが複数インストールされてしまうので、手動でインストールしています

コンパイルと実行
````sh
$ vi javascript.rs 
fn main() { println!("こんにちは"); }

$ rustc --target=asmjs-unknown-emscripten javascript.rs
$ node javascript.js
こんにちは
````

片付け
````sh
$ vi .profile
# emsdk_env.sh の行をコメントアウト
````

Emscripten が古いバージョンの Node.js を参照するので、使用しない時はコメントアウトしておきます（関数にしても良いと思います）

Emscripten の削除
````sh
$ vi .profile
# emsdk_env.sh の行を削除

$ rm -rf ~/Applications/Emscripten
````
