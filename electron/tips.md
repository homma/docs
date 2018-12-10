
## Electron Tips

### 既存のウェブページをデスクトップアプリにする

`Browser Window` で読み込む。

- https://github.com/electron/electron/blob/master/docs/api/browser-window.md

### Browser Window で JavaScript を実行する

まず `Web Contents` を取り出す。

- https://github.com/electron/electron/blob/master/docs/api/web-contents.md

- BrowserWindow.webContents

`Web Contents` に対して executeJavaScript を実行する。

- contents.executeJavaScript

### ES6 Modules を使う
