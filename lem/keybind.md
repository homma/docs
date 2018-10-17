## Lem のキーバインド

### キーバインドの実装
キーバインドの実装は [global-keymap](https://github.com/cxxxr/lem/search?q=*global-keymap*&unscoped_q=*global-keymap*) から探すことができます。

### キーバインドのヘルプ

キーバインドに対応するコマンドは、キーバインドのヘルプから確認することができます。

| キーバインド | コマンド | 意味 | 補足 |
| --- | --- | --- | --- |
| <kbd>C-x ?</kbd> | `describe-key` | キーバインドのヘルプ | <kbd>ctrl-x shift-/</kbd> |

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
| <kbd>M-g</kbd> | `goto-line` | 指定した番号の行に移動する | |
| <kbd>C-p</kbd> | `previous-line` | 一行上に移動する | <kbd>↑</kbd> |
| <kbd>C-n</kbd> | `next-line` | 一行下に移動する | <kbd>↓</kbd> |
| <kbd>C-f</kbd> | `forward-char` | 一文字右に移動する | <kbd>→</kbd> |
| <kbd>C-b</kbd> | `backward-char` | 一文字左に移動する | <kbd>←</kbd> |
| <kbd>C-a</kbd> | `move-to-beginninC-of-line` | 行頭に移動する | <kbd>Home</kbd> |
| <kbd>C-e</kbd> | `move-to-end-of-line` | 行末に移動する | <kbd>End</kbd> |
| <kbd>M-f</kbd> | `forward-word` | 次の単語に移動する | <kbd>C-Right</kbd> は動作確認できず |
| <kbd>M-b</kbd> | `previous-word` | 前の単語に移動する | <kbd>C-Left</kbd> は動作確認できず |
| <kbd>M-}</kbd> | `forward-paragraph` | 次のパラグラフに移動する | |
| <kbd>M-{</kbd> | `backward-paragraph` | 前のパラグラフに移動する | |
| <kbd>C-v</kbd> | `next-page` | 次のページに移動する | <kbd>PageDown</kbd> |
| <kbd>M-v</kbd> | `previous-page` | 前のページに移動する | <kbd>PageUp</kbd> |
| <kbd>C-x ]</kbd> | `next-page-char` | 次の改ページへ移動 | 次の `^L` へ移動 |
| <kbd>C-x [</kbd> | `previous-page-char` | 前の改ページへ移動 | 前の `^L` へ移動 |
| <kbd>M-<</kbd> | `move-to-beginninC-of-buffer` | バッファの一番最初に移動する | |
| <kbd>M-></kbd> | `move-to-end-of-buffer` | バッファの一番最後に移動する | |
| <kbd>M-m</kbd> | `back-to-indentation-command` | カーソル行の最初の空白ではない文字に移動する | |

<kbd>C-x [</kbd> は実質的にバッファの先頭に移動するキーとして使用可能です。  
<kbd>C-x ]</kbd> は実質的にバッファの末尾に移動するキーとして使用可能です。

### テキストの編集

#### 文字列補完
<kbd>/</kbd> と <kbd>Tab</kbd> で文字列を補完できます。

| キーバインド | コマンド | 意味 | 補足 |
| --- | --- | --- | --- |
| <kbd>C-x /</kbd> | `abbrev-with-pop-up-window` | 文字列補完ウィンドウを表示する | |
| <kbd>M-/</kbd> | `abbrev` | 文字列を補完する | 繰り返し入力すると補完候補が変わる | |
| <kbd>Tab</kbd> | `indent-line-and-complete-symbol` | インデントの挿入とシンボルの補完 | シンボルの補完はモードに依存 |

`a` の後ろにカーソルを配置して <kbd>C-x /</kbd> を入力すると `active` や `apropos` が候補として表示されます。  
`lisp-mode` で Common Lisp のキーワードなどの入力中に <kbd>Tab</kbd> を押すと補完候補が表示されます。

#### 文字の削除

削除系のキーバインドは以下の通りです。

| キーバインド | コマンド | 意味 | 補足 |
| --- | --- | --- | --- |
| <kbd>C-h</kbd> | `delete-previous-char` | カーソル直前の文字を削除 | `delete` |
| <kbd>C-d</kbd> | `delete-next-char` | カーソル位置の文字を削除 | `delete`
| <kbd>C-k</kbd> | `kill-line` | カーソル位置から行末までを消去 | `kill` 連続で実行すると複数行をコピー |
| <kbd>C-u \<n\> C-k</kbd> | `--` | n 行削除 | `kill` |
| <kbd>C-x C-o</kbd> | `delete-blank-lines` | 連続する空行をまとめて消去 | `kill` |
| <kbd>M-k</kbd> | `kill-paragraph` | カーソル位置以降のパラグラフを削除 | `kill` |
| <kbd>M-C-h</kbd> | `backward-delete-word` | カーソル直前の単語を消去 | `kill` <kbd>M-Backspace</kbd> |
| <kbd>M-d</kbd> | `delete-word` | カーソルの後ろの単語を消去 | `kill` <kbd>C-Delete</kbd> は動作確認できず |

#### ペースト

`kill` した内容は `yank` することができます。  

| キーバインド | コマンド | 意味 | 補足 |
| --- | --- | --- | --- |
| <kbd>C-y</kbd> | `yank` | ヤンク | ペースト |
| <kbd>M-y</kbd> | `yank-pop` | 履歴を遡ったヤンク | <kbd>C-y</kbd> の後に <kbd>M-y</kbd> を入力 |

`delete` した内容は `yank` できないので、必要な場合は `undo` します。

#### 改行とインデントの操作

| キーバインド | コマンド | 意味 | 補足 |
| --- | --- | --- | --- |
| <kbd>C-m</kbd> | `newline` | 改行 | <kbd>Return</kbd> |
| <kbd>C-o</kbd> | `open-line` | カーソルの後ろに改行を入力する | |
| <kbd>C-j</kbd> | `newline-and-indent` | 改行とインデント | プログラミングで使用する |
| <kbd>M-^</kbd> | `delete-indentation` | インデントを削除 | |

#### 範囲選択

<kbd>C-@</kbd> で範囲選択を行えます。

| キーバインド | コマンド | 意味 | 補足 |
| --- | --- | --- | --- |
| <kbd>C-@</kbd> | `mark-set` | マークを設定する | 範囲選択の開始 <kbd>C-Space</kbd> |
| <kbd>C-w</kbd> | `kill-region` | 範囲選択を終了し、選択範囲の文字列を消去する | `kill` |
| <kbd>M-w</kbd> | `copy-region` | 範囲選択を終了し、選択範囲の文字列をコピーする | |
| <kbd>C-x C-x</kbd> | `exchange-point-mark` | マークを設定した位置とカーソル位置を入れ替える | カーソル位置はマークが設定された位置に移動する |
| <kbd>C-x h</kbd> | `mark-set-whole-buffer` | バッファ全体をマークする | |

#### 矩形選択モード

<kbd>C-x Space</kbd> で矩形選択モードに移行します。

| キーバインド | コマンド | 意味 | 補足 |
| --- | --- | --- | --- |
| <kbd>C-x Space</kbd> | `rectangle-mark-mode` | 矩形選択モードを開始 | |
| <kbd>C-w</kbd> | `kill-region` | 範囲選択を終了し、選択範囲の文字列を消去する | `kill` |
| <kbd>M-w</kbd> | `copy-region` | 範囲選択を終了し、選択範囲の文字列をコピーする | |
| <kbd>C-o</kbd> | `rectangle-open` | 選択範囲に空白を挿入する | |
| <kbd>C-t</kbd> | `rectangle-string` | 選択範囲を指定した文字列で置き換える | |
| <kbd>C-x C-x</kbd> | `exchange-point-mark` | マークを設定した位置とカーソル位置を入れ替える | カーソル位置はマークが設定された位置に移動する |

#### undo と redo
- アンドゥは <kbd>C-\\</kbd>
- リドゥは <kbd>C-_</kbd>

ここは Emacs とは異なるようです。

| キーバインド | コマンド | 意味 | 補足 |
| --- | --- | --- | --- |
| <kbd>C-\\</kbd> | `undo` | アンドゥ | |
| <kbd>C-_</kbd> | `redo` | リドゥ | |

#### 繰り返し
同じ操作を繰り返し実行する際に使用できます。

| キーバインド | コマンド | 意味 | 補足 |
| --- | --- | --- | --- |
| <kbd>C-u \<n\></kbd> | `universal-argument` | 繰り返し実行 | <kbd>C-u \<n\></kbd> の後にキーを入力すると `n` 回実行される |
| <kbd>C-u -</kbd> | `universal-argument-minus` | 回数にマイナスを指定する | |
| <kbd>M-\<n\></kbd> | `universal-argument-0` | n 回繰り返し | n は 0 ~ 9 |

<kbd>C-u</kbd> を連続して入力すると、繰り返す回数が増えます。

#### 検索

Emacs 系といえばインクリメンタルサーチ。

| キーバインド | コマンド | 意味 | 補足 |
| --- | --- | --- | --- |
| <kbd>C-s</kbd> | `isearch-forward` | 前方検索 | 大文字小文字を区別しない |
| <kbd>C-r</kbd> | `isearch-backward` | 後方検索 | 大文字小文字を区別しない |
| <kbd>C-M-s</kbd> | `isearch-forward-regexp` | 正規表現で前方検索 | |
| <kbd>C-M-r</kbd> | `isearch-backward-regexp` | 正規表現で後方検索 | |
| <kbd>M-s _</kbd> | `isearch-forward-symbol` | 前方へのシンボル検索 | プログラミングで使用 |
| <kbd>M-s M-_</kbd> | `isearch-backward-symbol` | 後方へのシンボル検索 | プログラミングで使用 |
| <kbd>M-s .</kbd> | `isearch-forward-symbol-at-point` | カーソルの近くにあるシンボルを前方検索 | |

#### 置換

| キーバインド | コマンド | 意味 | 補足 |
| --- | --- | --- | --- |
| <kbd>M-%</kbd> | `query-replace` | 置換 | |

ミニバッファで `Before` に置換前の単語、`After` に置換後の単語を入力し、`y` を入力すると文字列を置換できます。

#### 文字変換

大文字・小文字の変換などをサポートしています。

| キーバインド | コマンド | 意味 | 補足 |
| --- | --- | --- | --- |
| <kbd>C-t</kbd> | `transpose-characters` | 前後の文字を入れ替える | |
| <kbd>M-u</kbd> | `uppercase-word` | 単語を大文字に変換する | |
| <kbd>M-l</kbd> | `lowercase-word` | 単語を小文字に変換する | |
| <kbd>C-x C-u</kbd> | `uppercase-region` | 選択範囲の文字列を大文字に変換する | |
| <kbd>C-x C-l</kbd> | `downcase-region` | 選択範囲の文字列を小文字に変換する | |
| <kbd>M-c</kbd> | `capitalize-word` | 先頭の文字を大文字に、後続の文字を小文字に変換する | |
| <kbd>M-Space</kbd> | `just-one-space` | 複数の連続した空白文字を一つの空白文字にまとめる | |

#### 文字入力

特殊な文字列入力キーです。

| キーバインド | コマンド | 意味 | 補足 |
| --- | --- | --- | --- |
| <kbd>C-q</kbd> | `quoted-insert` | 入力をエスケープしない | <kbd>Ctrl-v</kbd> のような感じ |
| <kbd>M-(</kbd> | `insert-()` | `()` を入力する | |
| <kbd>M-)</kbd> | `move-over-)` | 閉じ括弧を探して改行とインデントを挿入する | |

### キーボードマクロ

一連の操作をマクロとして記録し、再実行することができます。

| キーバインド | コマンド | 意味 | 補足 |
| --- | --- | --- | --- |
| <kbd>C-x (</kbd> | `kbdmacro-start` | キーボードマクロの記録開始 | |
| <kbd>C-x )</kbd> | `kbdmacro-end` | キーボードマクロの記録終了 | |
| <kbd>C-x e</kbd> | `kbdmacro-execute` | キーボードマクロの実行 | |

### Lisp プログラムの操作

Lem はフルセットの Common Lisp 処理系で実装されているため、Lisp プログラムの実行機能や、S 式の編集機能がデフォルトで備わっています。

#### プログラムの評価

| キーバインド | コマンド | 意味 | 補足 |
| --- | --- | --- | --- |
| <kbd>M-:</kbd> | `self-lisp-eval-string` | 指定した Lisp プログラムを実行する | プログラムはミニバッファで指定する |
| <kbd>C-x C-e</kbd> | `self-lisp-eval-last-expression` | カーソル直前の Lisp プログラムの評価 | 実行結果はミニバッファに出力される |

#### S 式の操作

| キーバインド | コマンド | 意味 | 補足 |
| --- | --- | --- | --- |
| <kbd>M-C-f</kbd> | `forward-sexp` | 次の S 式へ移動する | |
| <kbd>M-C-b</kbd> | `backward-sexp` | 前の S 式へ移動する | |
| <kbd>M-C-n</kbd> | `forward-list` | 次のリストへ移動する | |
| <kbd>M-C-p</kbd> | `backward-list` | 前のリストへ移動する | |
| <kbd>M-C-d</kbd> | `down-list` | 内側のリストへ移動する | |
| <kbd>M-C-u</kbd> | `backward-up-list` | 外側のリストへ移動する | |
| <kbd>C-M-@</kbd> | `mark-sexp` | S 式にマークを設定する | 英語キーボードでは使用できない |
| <kbd>M-C-k</kbd> | `kill-sexp` | S 式を消去する | `kill` |
| <kbd>M-C-t</kbd> | `transpose-sexps` | 前の S 式と入れ替える | |

<kbd>M-C-@</kbd> は英語キーボードでは <kbd>M-2</kbd> と重複しているため、実行することができません。

### バッファの操作

Emacs と同様、Lem も複数のバッファを扱うことができます。

| キーバインド | コマンド | 意味 | 補足 |
| --- | --- | --- | --- |
| <kbd>M-=</kbd> | `count-words` | バッファの行数、単語数、文字数を集計する | |
| <kbd>C-x C-f</kbd> | `find-file` | ファイルを開く | 後述 |
| <kbd>C-x C-r</kbd> | `read-file` | ファイルを読み取り専用で開く | |
| <kbd>C-x C-q</kbd> | `toggle-read-only` | 読み取り専用・書き込み可能の切り替え | |
| <kbd>C-x Tab</kbd> | `insert-file` | 現在のバッファに指定したファイルの中身を挿入する | |
| <kbd>C-x C-b</kbd> | `list-buffers` | `*Buffer Menu*` を表示する | |
| <kbd>C-x b</kbd> | `select-buffer` | ウィンドウに表示するバッファを変更する | |
| <kbd>C-x Left</kbd> | `previous-buffer` | 前のバッファに移動する | |
| <kbd>C-x Right</kbd> | `next-buffer` | 次のバッファに移動する | |
| <kbd>C-x k</kbd> | `kill-buffer` | バッファを閉じる | |
| <kbd>C-x C-s</kbd> | `save-buffer` | バッファを保存する | |
| <kbd>C-x C-w</kbd> | `write-file` | 指定したファイル名でバッファを保存する | |
| <kbd>C-x s</kbd> | `save-some-buffers` | 保存が必要なファイルを保存するか順番に確認する | |

#### <kbd>C-x C-f</kbd> の動作
- 存在しないファイル名を指定すると新規にファイルを作成できます
- すでに開かれているファイル名を指定すると、そのファイルのバッファに移動します

#### `*Buffer Menu*` の操作

| キーバインド | コマンド | 意味 | 補足 |
| --- | --- | --- | --- |
| <kbd>q</kbd> | `quit-window` | メニューを終了する | |
| <kbd>d</kbd> | `menu-delete` | バッファを削除する | |
| <kbd>Return</kbd> | `menu-select-this-window` | バッファを選択する | |
| <kbd>C-x o</kbd> | `other-window` | 他のウィンドウへ移動する | |
| <kbd>o</kbd> | `menu-select-switch-other-window` | 選択したバッファを他のウィンドウで開く | |
| <kbd>n</kbd> | `menu-next-line` | メニューの次の項目に移動する | |
| <kbd>p</kbd> | `menu-previous-line` | メニューの前の項目に移動する | |
| <kbd>m</kbd> | `menu-mark-and-next-line` | マークをつける | |
| <kbd>u</kbd> | `menu-unmark-and-next-line` | マークを解除する | |
| <kbd>U</kbd> | `menu-unmark-and-previous-line` | マークを解除する | |
| <kbd>g</kbd> | `menu-update` | メニューを更新する | |

### ウィンドウの操作

複数のウィンドウを表示して操作することができます。

#### ウィンドウ分割

| キーバインド | コマンド | 意味 | 補足 |
| --- | --- | --- | --- |
| <kbd>C-x 0</kbd> | `delete-current-window` | 現在カーソルのあるウィンドウを閉じる | |
| <kbd>C-x 1</kbd> | `delete-other-windows` | 他のウィンドウを閉じる | |
| <kbd>C-x 2</kbd> | `split-active-window-vertically` | 上下にウィンドウを分割する | |
| <kbd>C-x 3</kbd> | `split-active-window-horizontally` | 左右にウィンドウを分割する | |
| <kbd>C-x o</kbd> | `other-window` | 他のウィンドウに移動する | |
| <kbd>M-o</kbd> | `other-window` | 他のウィンドウに移動する | |
| <kbd>C-x ^</kbd> | `grow-window` | ウィンドウサイズを上下に大きくする | |
| <kbd>C-x C-z</kbd> | `shrink-window` | ウィンドウサイズを上下に小さくする | |
| <kbd>C-x }</kbd> | `grow-window-horizontally` | ウィンドウサイズを左右に大きくする | |
| <kbd>C-x {</kbd> | `shrink-window-horizontally` | ウィンドウサイズを左右に小さくする | |
| <kbd>C-x 4 f</kbd> | `find-file-other-window` | 指定したファイルを新規ウィンドウで開く | |
| <kbd>C-x 4 r</kbd> | `read-file-other-window` | 指定したファイルを読み取り専用で新規ウィンドウで開く | |
| <kbd>C-x 4 b</kbd> | `select-buffer-other-window` | 指定したバッファを新規ウィンドウで開く | |

#### 画面スクロール

| キーバインド | コマンド | 意味 | 補足 |
| --- | --- | --- | --- |
| <kbd>C-l</kbd> | `recenter` | カーソル位置が中央になるようにスクロールする | |
| <kbd>C-u C-v</kbd> | `--` | 4  行上にスクロールする | |
| <kbd>C-u \<n\> C-v</kbd> | `--` | n  行上にスクロールする | |
| <kbd>C-u M-v</kbd> | `--` | 4  行下にスクロールする | |
| <kbd>C-u \<n\> M-v</kbd> | `--` | n  行下にスクロールする | |

### エディタの操作
- <kbd>C-g</kbd> は非常に重要なキーです
- エディタの終了は <kbd>C-x C-c</kbd> です

| キーバインド | コマンド | 意味 | 補足 |
| --- | --- | --- | --- |
| <kbd>C-g</kbd> | `keyboard-quit` | 中断 | |
| <kbd>C-x C-c</kbd> | `exit-lem` | Lem を終了させる | |

#### <kbd>C-g</kbd> の機能

- 入力途中のコマンドを終了
- 検索を終了
- 時間がかかっているコマンドを停止

### コマンドの入力

Lem のエディタ拡張機能にアクセスするキーです。

| キーバインド | コマンド | 意味 | 補足 |
| --- | --- | --- | --- |
| <kbd>C-x</kbd> | `--` | キープリフィクス | 複合キーバインドの先頭部分 |
| <kbd>M-x</kbd> | `execute-command` | コマンド名でコマンドを指定して実行する | |

コマンド名はタブキーで補完することができます。

### その他の操作

| キーバインド | コマンド | 意味 | 補足 |
| --- | --- | --- | --- |
| <kbd>C-x @</kbd> | `pipe-command` | ミニバッファに入力した OS コマンドを実行し、結果を取得する | 外部コマンドの実行 |

### 空いているキー

以下のキーには、現時点ではコマンドが割り当てられていません。

#### Ctrl

| キーバインド |
| --- |
| <kbd>C-z</kbd> |
| <kbd>C-c</kbd> |

#### Meta

| キーバインド |
| --- |
| <kbd>M-p</kbd> |
| <kbd>M-n</kbd> |
| <kbd>M-r</kbd> |

### 入力ができないキー

以下のキーバインドは macOS では使用できませんでした。  
OS の設定の変更を行った場合や、他の OS では使用可能かもしれません。

| キーバインド | コマンド | 動き |
| --- | --- | --- |
| <kbd>C-M-@</kbd> | `mark-sexp` | 英語キーボードでは <kbd>M-2</kbd> と衝突 |
| <kbd>C-Space</kbd> | `mark-set` | マークを設定する | 日本語変換キーと衝突 |
| <kbd>C-Right</kbd> | `forward-word` | 次の単語に移動する |
| <kbd>C-Left</kbd> | `previous-word` | 前の単語に移動する |
| <kbd>C-Delete</kbd> | `delete-word` | カーソルの後ろの単語を消去 |
| <kbd>C-Down</kbd> | `scroll-down` | macOS のショートカットと衝突 |
| <kbd>C-Up</kbd> | `scroll-up` | macOS のショートカットと衝突 |

### 不明なキー

以下のキーは使い方がよくわかっていません。  

| キーバインド | コマンド | 動き |
| --- | --- | --- |
| <kbd>C-]</kbd> | `--` | `Interrupt` が発生する |
| <kbd>M-~</kbd> | `unmark-buffer` | |
| <kbd>C-x #</kbd> | `filter-buffer` | |

#### ソースリスト

| キーバインド | コマンド | 動き |
| --- | --- | --- |
| <kbd>C-x n</kbd> | `sourcelist-next` | `--` |
| <kbd>C-x C-n</kbd> | `sourcelist-next` | `--` |
| <kbd>C-x p</kbd> | `sourcelist-previous` | `--` |
| <kbd>C-x C-p</kbd> | `sourcelist-previous` | `--` |

### Lem には存在しないキー

以下は Emacs には存在していて Lem に存在しないキーの一部です。

| キーバインド | Emacs での意味 |
| --- | --- |
| <kbd>C-h</kbd> | ヘルプ |
| <kbd>C-z</kbd> | エディタを一時停止する |
| <kbd>M-C-v</kbd> | 他のウィンドウをスクロールさせる |
| <kbd>Esc Esc Esc</kbd> | 抜け出し |
