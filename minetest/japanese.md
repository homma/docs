# 日本語入力について

- チャットやコマンドのコンソールへの日本語直接入力はできない
- Irrlicht Engine レベルでの修正が必要

- Ctrl-V でペーストすることは可能
- テキストエディタで日本語を入力し、Minetest のコンソールにペーストすれば良い

- 自動でコピー・ペーストするプログラムを作成しても良い

- AppleScript は日本語の直接入力ができない
- 日本語入力のためにクリップボードからコピペする
- https://apple.stackexchange.com/questions/288536/is-it-possible-to-keystroke-special-characters-in-applescript
- スクリプトの実行には機能拡張のセキュリティとプライバシーからアクセシビリティに対してキー送信を許可してあげる必要がある

- 以下のスクリプトで日本語を入力できることを確認した
````
set message to "日本語メッセージ"

tell application "minetest"
  activate
end tell

tell application "System Events"
  tell process "minetest"
    keystroke "T"
    set the clipboard to message
    keystroke "v" using control down
  end tell
end tell
````

- https://developer.apple.com/library/archive/documentation/LanguagesUtilities/Conceptual/MacAutomationScriptingGuide/AutomatetheUserInterface.html
