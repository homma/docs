## Lem のキーバインド

以前から興味のあった [Lem](https://github.com/cxxxr/lem) に入門したので、キーバインドをまとめてみました。  

普段は主に Vim を使用しているため、Emacs 系のエディタには詳しくありません。  
もし間違い等がありましたらごめんなさい。

### キーバインドの実装
キーバインドの実装は [global-keymap](https://github.com/cxxxr/lem/search?q=*global-keymap*&unscoped_q=*global-keymap*) から探すことができます。

### キーバインドのヘルプ

キーバインドに対応するコマンドは、キーバインドのヘルプから確認することができます。

| キーバインド | コマンド | 意味 | 補足 |
| --- | --- | --- | --- |
| `C-x ?` | `describe-key` | キーバインドのヘルプ | `ctrl-x shift-:` |

### カーソルの移動

カーソルを移動させるキーは以下の通りです。

````
        M-v
         p
         ^
         |
 a  b <--+--> f  e
         |
         V
         n
         v
````

| キーバインド | コマンド | 意味 | 補足 |
| ----- | --- | --- | --- |
| `M-g` | `goto-line` | 指定した番号の行に移動する | |
| `C-p` | `previous-line` | 一行上に移動する | `↑` |
| `C-n` | `next-line` | 一行下に移動する | `↓` |
| `C-f` | `forward-char` | 一文字右に移動する | `→` |
| `C-b` | `backward-char` | 一文字左に移動する | `←` |
| `C-a` | `move-to-beginning-of-line` | 行頭に移動する | `Home` |
| `C-e` | `move-to-end-of-line` | 行末に移動する | `End` |
| `M-f` | `forward-word` | 次の単語に移動する | `C-Right` は動作確認できず |
| `M-b` | `previous-word` | 前の単語に移動する | `C-Left` は動作確認できず |
| `M-}` | `forward-paragraph` | 次のパラグラフに移動する | |
| `M-{` | `backward-paragraph` | 前のパラグラフに移動する | |
| `C-v` | `next-page` | 次のページに移動する | `PageDown` |
| `M-v` | `previous-page` | 前のページに移動する | `PageUp` |
| `C-x ]` | `next-page-char` | 次の改ページへ移動 | 次の `^L` へ移動 |
| `C-x [` | `previous-page-char` | 前の改ページへ移動 | 前の `^L` へ移動 |
| `M-<` | `move-to-beginning-of-buffer` | バッファの一番最初に移動する | |
| `M->` | `move-to-end-of-buffer` | バッファの一番最後に移動する | |
| `M-m` | `back-to-indentation-command` | カーソル行の最初の空白ではない文字に移動する | |

`C-x [` は実質的にバッファの先頭に移動するキーとして使用可能です。  
`C-x ]` は実質的にバッファの末尾に移動するキーとして使用可能です。

### テキストの編集

#### 文字列補完
`/` と `Tab` で文字列を補完できます。

| キーバインド | コマンド | 意味 | 補足 |
| --- | --- | --- | --- |
| `C-x /` | `abbrev-with-pop-up-window` | 文字列補完ウィンドウを表示する | |
| `M-/` | `abbrev` | 文字列を補完する | 繰り返し入力すると補完候補が変わる | |
| `Tab` | `indent-line-and-complete-symbol` | インデントの挿入とシンボルの補完 | シンボルの補完はモードに依存 |

`a` の後ろにカーソルを配置して `C-x /` を入力すると `active` や `apropos` が候補として表示されます。  
`lisp-mode` で Common Lisp のキーワードなどの入力中に `Tab` を押すと補完候補が表示されます。

#### 文字の削除

削除系のキーバインドは以下の通りです。

| キーバインド | コマンド | 意味 | 補足 |
| --- | --- | --- | --- |
| `C-h` | `delete-previous-char` | カーソル直前の文字を削除 | `delete` |
| `C-d` | `delete-next-char` | カーソル位置の文字を削除 | `delete`
| `C-k` | `kill-line` | カーソル位置から行末までを消去 | `kill` 連続で実行すると複数行をコピー |
| `C-u <n> C-k` | `--` | n 行削除 | `kill` |
| `C-x C-o` | `delete-blank-lines` | 連続する空行をまとめて消去 | `kill` |
| `M-k` | `kill-paragraph` | カーソル位置以降のパラグラフを削除 | `kill` |
| `M-C-h` | `backward-delete-word` | カーソル直前の単語を消去 | `kill` `M-Backspace` |
| `M-d` | `delete-word` | カーソルの後ろの単語を消去 | `kill` `C-Delete` は動作確認できず |

#### ペースト

`kill` した内容は `yank` することができます。  

| キーバインド | コマンド | 意味 | 補足 |
| --- | --- | --- | --- |
| `C-y` | `yank` | ヤンク | ペースト |
| `M-y` | `yank-pop` | 履歴を遡ったヤンク | `C-y` の後に `M-y` を入力 |

`delete` した内容は `yank` できないので、必要な場合は `undo` します。

#### 改行とインデントの操作

| キーバインド | コマンド | 意味 | 補足 |
| --- | --- | --- | --- |
| `C-m` | `newline` | 改行 | `Return` |
| `C-o` | `open-line` | カーソルの後ろに改行を入力する | |
| `C-j` | `newline-and-indent` | 改行とインデント | プログラミングで使用する |
| `M-^` | `delete-indentation` | インデントを削除 | |

#### 範囲選択

`C-@` で範囲選択を行えます。

| キーバインド | コマンド | 意味 | 補足 |
| --- | --- | --- | --- |
| `C-@` | `mark-set` | マークを設定する | 範囲選択の開始 `C-Space` |
| `C-w` | `kill-region` | 範囲選択を終了し、選択範囲の文字列を消去する | `kill` |
| `M-w` | `copy-region` | 範囲選択を終了し、選択範囲の文字列をコピーする | |
| `C-x C-x` | `exchange-point-mark` | マークを設定した位置とカーソル位置を入れ替える | カーソル位置はマークが設定された位置に移動する |
| `C-x h` | `mark-set-whole-buffer` | バッファ全体をマークする | |

#### 矩形選択モード

`C-x Space` で矩形選択モードに移行します。

| キーバインド | コマンド | 意味 | 補足 |
| --- | --- | --- | --- |
| `C-x Space` | `rectangle-mark-mode` | 矩形選択モードを開始 | |
| `C-w` | `kill-region` | 範囲選択を終了し、選択範囲の文字列を消去する | `kill` |
| `M-w` | `copy-region` | 範囲選択を終了し、選択範囲の文字列をコピーする | |
| `C-o` | `rectangle-open` | 選択範囲に空白を挿入する | |
| `C-t` | `rectangle-string` | 選択範囲を指定した文字列で置き換える | |
| `C-x C-x` | `exchange-point-mark` | マークを設定した位置とカーソル位置を入れ替える | カーソル位置はマークが設定された位置に移動する |

#### undo / redo
- アンドゥは `C-\`
- リドゥは `C-_`

ここは Emacs とは異なるようです。

| キーバインド | コマンド | 意味 | 補足 |
| --- | --- | --- | --- |
| `C-\` | `undo` | アンドゥ | |
| `C-_` | `redo` | リドゥ | |

#### 繰り返し
同じ操作を繰り返し実行する際に使用できます。

| キーバインド | コマンド | 意味 | 補足 |
| --- | --- | --- | --- |
| `C-u <n>` | `universal-argument` | 繰り返し実行 | `C-u <n>` の後にキーを入力すると `n` 回実行される |
| `C-u -` | `universal-argument-minus` | 回数にマイナスを指定する | |
| `M-<n>` | `universal-argument-0` | n 回繰り返し | n は 0 ~ 9 |

`C-u` を連続して入力すると、繰り返す回数が増えます。

#### 検索

Emacs 系といえばインクリメンタルサーチ。

| キーバインド | コマンド | 意味 | 補足 |
| --- | --- | --- | --- |
| `C-s` | `isearch-forward` | 前方検索 | 大文字小文字を区別しない |
| `C-r` | `isearch-backward` | 後方検索 | 大文字小文字を区別しない |
| `C-M-s` | `isearch-forward-regexp` | 正規表現で前方検索 | |
| `C-M-r` | `isearch-backward-regexp` | 正規表現で後方検索 | |
| `M-s _` | `isearch-forward-symbol` | 前方へのシンボル検索 | プログラミングで使用 |
| `M-s M-_` | `isearch-backward-symbol` | 後方へのシンボル検索 | プログラミングで使用 |
| `M-s .` | `isearch-forward-symbol-at-point` | カーソルの近くにあるシンボルを前方検索 | |

#### 文字変換

大文字・小文字の変換などをサポートしています。

| キーバインド | コマンド | 意味 | 補足 |
| --- | --- | --- | --- |
| `C-t` | `transpose-characters` | 前後の文字を入れ替える | |
| `M-u` | `uppercase-word` | 単語を大文字に変換する | |
| `M-l` | `lowercase-word` | 単語を小文字に変換する | |
| `C-x C-u` | `uppercase-region` | 選択範囲の文字列を大文字に変換する | |
| `C-x C-l` | `downcase-region` | 選択範囲の文字列を小文字に変換する | |
| `M-c` | `capitalize-word` | 先頭の文字を大文字に、後続の文字を小文字に変換する | |
| `M-Space` | `just-one-space` | 複数の連続した空白文字を一つの空白文字にまとめる | |

### キーボードマクロ

一連の操作をマクロとして記録し、再実行することができます。

| キーバインド | コマンド | 意味 | 補足 |
| --- | --- | --- | --- |
| `C-x (` | `kbdmacro-start` | キーボードマクロの記録開始 | |
| `C-x )` | `kbdmacro-end` | キーボードマクロの記録終了 | |
| `C-x e` | `kbdmacro-execute` | キーボードマクロの実行 | |

#### 文字入力

特殊な文字列入力キーです。

| キーバインド | コマンド | 意味 | 補足 |
| --- | --- | --- | --- |
| `C-q` | `quoted-insert` | 入力をエスケープしない | `Ctrl-v` のような感じ |
| `M-(` | `insert-()` | `()` を入力する | |
| `M-)` | `move-over-)` | 閉じ括弧を探して改行とインデントを挿入する | |

### Lisp プログラムの操作

Lem はフルセットの Common Lisp 処理系で実装されているため、Lisp プログラムの実行機能や、S 式の編集機能がデフォルトで備わっています。

#### プログラムの評価

| キーバインド | コマンド | 意味 | 補足 |
| --- | --- | --- | --- |
| `M-:` | `self-lisp-eval-string` | 指定した Lisp プログラムを実行する | プログラムはミニバッファで指定する |
| `C-x C-e` | `self-lisp-eval-last-expression` | カーソル直前の Lisp プログラムの評価 | 実行結果はミニバッファに出力される |

#### S 式の操作

| キーバインド | コマンド | 意味 | 補足 |
| --- | --- | --- | --- |
| `M-C-f` | `forward-sexp` | 次の S 式へ移動する | |
| `M-C-b` | `backward-sexp` | 前の S 式へ移動する | |
| `M-C-n` | `forward-list` | 次のリストへ移動する | |
| `M-C-p` | `backward-list` | 前のリストへ移動する | |
| `M-C-d` | `down-list` | 内側のリストへ移動する | |
| `M-C-u` | `backward-up-list` | 外側のリストへ移動する | |
| `C-M-@` | `mark-sexp` | S 式にマークを設定する | 英語キーボードでは使用できない |
| `M-C-k` | `kill-sexp` | S 式を消去する | `kill` |
| `M-C-t` | `transpose-sexps` | 前の S 式と入れ替える | |

`M-C-@` は英語キーボードでは `M-2` と重複しているため、実行することができません。

### バッファの操作

Emacs と同様、Lem も複数のバッファを扱うことができます。

| キーバインド | コマンド | 意味 | 補足 |
| --- | --- | --- | --- |
| `M-=` | `count-words` | バッファの行数、単語数、文字数を集計する | |
| `C-x C-f` | `find-file` | ファイルを開く | 後述 |
| `C-x C-r` | `read-file` | ファイルを読み取り専用で開く | |
| `C-x C-q` | `toggle-read-only` | 読み取り専用・書き込み可能の切り替え | |
| `C-x Tab` | `insert-file` | 現在のバッファに指定したファイルの中身を挿入する | |
| `C-x C-b` | `list-buffers` | `*Buffer Menu*` を表示する | |
| `C-x b` | `select-buffer` | ウィンドウに表示するバッファを変更する | |
| `C-x Left` | `previous-buffer` | 前のバッファに移動する | |
| `C-x Right` | `next-buffer` | 次のバッファに移動する | |
| `C-x k` | `kill-buffer` | バッファを閉じる | |
| `C-x C-s` | `save-buffer` | バッファを保存する | |
| `C-x C-w` | `write-file` | 指定したファイル名でバッファを保存する | |
| `C-x s` | `save-some-buffers` | 保存が必要なファイルを保存するか順番に確認する | |

#### `C-x C-f` の動作
- 存在しないファイル名を指定すると新規にファイルを作成できる
- すでに開かれているファイル名を指定すると、そのファイルのバッファに移動する

#### `*Buffer Menu*` の操作
- `d` でバッファを閉じる

### ウィンドウの操作

複数のウィンドウを表示して操作することができます。

#### ウィンドウ分割

| キーバインド | コマンド | 意味 | 補足 |
| --- | --- | --- | --- |
| `C-x 0` | `delete-current-window` | 現在カーソルのあるウィンドウを閉じる | |
| `C-x 1` | `delete-other-windows` | 他のウィンドウを閉じる | |
| `C-x 2` | `split-active-window-vertically` | 上下にウィンドウを分割する | |
| `C-x 3` | `split-active-window-horizontally` | 左右にウィンドウを分割する | || `C-x o` | `other-window` | 他のウィンドウに移動する | |
| `M-o` | `other-window` | 他のウィンドウに移動する | |
| `C-x ^` | `grow-window` | ウィンドウサイズを上下に大きくする | |
| `C-x C-z` | `shrink-window` | ウィンドウサイズを上下に小さくする | |
| `C-x }` | `grow-window-horizontally` | ウィンドウサイズを左右に大きくする | |
| `C-x {` | `shrink-window-horizontally` | ウィンドウサイズを左右に小さくする | |
| `C-x 4 f` | `find-file-other-window` | 指定したファイルを新規ウィンドウで開く | |
| `C-x 4 r` | `read-file-other-window` | 指定したファイルを読み取り専用で新規ウィンドウで開く | |
| `C-x 4 b` | `select-buffer-other-window` | 指定したバッファを新規ウィンドウで開く | |

#### 画面スクロール

| キーバインド | コマンド | 意味 | 補足 |
| --- | --- | --- | --- |
| `C-l` | `recenter` | カーソル位置が中央になるようにスクロールする | |
| `C-u C-v` | `--` | 4  行上にスクロールする | |
| `C-u <n> C-v` | `--` | n  行上にスクロールする | |
| `C-u M-v` | `--` | 4  行下にスクロールする | |
| `C-u <n> M-v` | `--` | n  行下にスクロールする | |

### エディタの操作
- `C-g` は非常に重要なキーです
- エディタの終了は `C-x C-c` です

| キーバインド | コマンド | 意味 | 補足 |
| --- | --- | --- | --- |
| `C-g` | `keyboard-quit` | 中断 | |
| `C-x C-c` | `exit-lem` | Lem を終了させる | |

#### `C-g` の機能

- 入力途中のコマンドを終了
- 検索を終了
- 時間がかかっているコマンドを停止

### コマンドの入力

Lem のエディタ拡張機能にアクセスするキーです。

| キーバインド | コマンド | 意味 | 補足 |
| --- | --- | --- | --- |
| `C-x` | `--` | キープリフィクス | 複合キーバインドの先頭部分 |
| `M-x` | `execute-command` | コマンド名でコマンドを指定して実行する | |

コマンド名はタブキーで補完することができます。

### 空いているキー

以下のキーには、現時点ではコマンドが割り当てられていません。

#### Ctrl

| キーバインド |
| --- |
| `C-z` |
| `C-c` |

#### Meta

| キーバインド |
| --- |
| `M-p` |
| `M-n` |

### 入力ができないキー

以下のキーバインドは macOS では使用できませんでした。  
OS の設定の変更を行った場合や、他の OS では使用可能かもしれません。

| キーバインド | コマンド | 動き |
| --- | --- | --- |
| `C-M-@` | `mark-sexp` | 英語キーボードでは `M-2` と衝突 |
| `C-Space` | `mark-set` | マークを設定する | 日本語変換キーと衝突 |
| `C-Right` | `forward-word` | 次の単語に移動する |
| `C-Left` | `previous-word` | 前の単語に移動する |
| `C-Delete` | `delete-word` | カーソルの後ろの単語を消去 |
| `C-Down` | `scroll-down` | macOS のショートカットと衝突 |
| `C-Up` | `scroll-up` | macOS のショートカットと衝突 |

### 不明なキー

以下のキーは使い方がよくわかっていません。  
`pipe-command` は便利そうです。

| キーバインド | コマンド | 動き |
| --- | --- | --- |
| `C-]` | `--` | `Interrupt` が発生する |
| `M-~` | `unmark-buffer` | |
| `C-x #` | `filter-buffer` | |
| `C-x @` | `pipe-command` | 外部コマンドの実行 |

#### ソースリスト

| キーバインド | コマンド | 動き |
| --- | --- | --- |
| `C-x n` | `sourcelist-next` | `--` |
| `C-x C-n` | `sourcelist-next` | `--` |
| `C-x p` | `sourcelist-previous` | `--` |
| `C-x C-p` | `sourcelist-previous` | `--` |

### Lem には存在しないキー

以下は Emacs には存在していて Lem に存在しないキーの一部です。

| キーバインド | Emacs での意味 |
| --- | --- |
| `C-h` | ヘルプ |
| `C-z` | エディタを一時停止する |
| `M-C-v` | 他のウィンドウをスクロールさせる |
| `Esc Esc Esc` | 抜け出し |