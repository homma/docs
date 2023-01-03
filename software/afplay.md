
afplay コマンドで音楽ファイルを再生できる。
一時停止と再開は以下のコマンドで実行できる。

````
alias afstop="kill -SIGSTOP $(pgrep afplay)"
alias afcont="kill -SIGCONT $(pgrep afplay)"
````
