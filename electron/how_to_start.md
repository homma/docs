
## Electron のプロジェクトを開始する

### package.json を用意する

npm init で作成することも可能。

````sh
$ mkdir myapp
$ cd myapp
$ npm init
````

あるいは、手動で `package.json` ファイルを用意することも可能。

````json
{
  "name": "myapp",
  "main": "main.js",
  "scripts": {
    "start": "electron ."
  }
}
````

### Electron をインストールする

````sh
$ npm install --save-dev electron
````

`package.json` に以下のようなエントリーが追加されます。

````
  "devDependencies": {
    "electron": "^3.0.10"
  }
````

`package-lock.json` が作成されます。

### プログラムの作成

````javascript
const { app, BrowserWindow } = require('electron');

const url = "http://www.google.com/";

const createWindow = (width, height) => {

  const win = new BrowserWindow({width: width, height: height});

  win.loadURL(url);

};

const main = () => {

  const width = 1000;
  const height = 800;

  createWindow(width, height);

};

app.on('ready', main);
````

### プログラムの実行

````sh
$ npm start
````

### 後片付け

````
$ cd ~/Library/Application Support/
$ rm -rf <App Name>
````
