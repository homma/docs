
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

- 使い方不明
- 現時点では対応していないと思われる

### アプリケーションのデータの格納場所

クッキーやローカルストレージなどは以下の場所に格納されます。

- `~/Library/Application Support/<App Name>`

アプリケーションを削除する際は、ここに含まれるデータも削除する必要がある。

### jQuery がロードされていないというエラーが出た場合の対処方法

- https://electronjs.org/docs/faq

`nodeIntegration` を `false` に設定する。

````
  const properties = {
    width: width,
    height: height,

    webPreferences: {
      nodeIntegration: false
    }

  };

  const win = new BrowserWindow(properties);
````