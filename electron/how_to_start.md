
## Electron のプロジェクトを開始する

### package.json を用意する

````sh
$ mkdir myapp
$ cd myapp
$ npm init
````

### Electron をインストールする

````sh
$ npm install --save-dev electron
````

package.json に以下のようなエントリーが追加されます。

````
  "devDependencies": {
    "electron": "^3.0.10"
  }
````


### 後片付け

````
$ cd ~/Library/Application Support/
$ rm -rf <App Name>
````
