## Firebase: アクセス確認

### 事前準備

### Firebase プロジェクトの作成

適当に名前をつけてプロジェクトを作成します。  
詳細は省略。

### プロジェクト ID と API キーの取得

`Project Overview` の右にある歯車アイコン（`Settings`）の「プロジェクトの設定」で確認できます。

### Firestore の作成

- プロジェクトページ
- `Database` ペイン
- 「データベースの作成」
- 「テストモードで開始」
- 「有効にする」

### アクセステスト

````sh
$ mkdir access_test
$ cd $_
$ npm install firebase
$ vi access_test.mjs
$ node --experimental-modules access_test.mjs
````

#### access_test.mjs

````javascript
import firebase from "firebase/app";
import "firebase/firestore";

const api_key = "My API Key";
const proj_id = "My Project ID";

const config = {
  apiKey: api_key,
  authDomain: `${proj_id}.firebaseapp.com`,
  projectId: proj_id
};

const test = async () => {
  firebase.initializeApp(config);

  const db = firebase.firestore();

  // surpress the Beta warning
  const firestore = firebase.firestore();
  const settings = { timestampsInSnapshots: true };
  firestore.settings(settings);

  console.log("add collection");
  const coll_name = "MyCollection";
  const coll = db.collection(coll_name);

  const data = {
    col1: "col1_data",
    col2: "col2_data"
  };

  console.log("add document");
  const doc = await coll.add(data);

  console.log("get document");
  const ret = await doc.get();
  console.log(ret.data());

  console.log("delete document");
  await doc.delete();

  console.log("done.");
  // cannot delete collection from JavaScript
};

(async () => {
  await test();
  process.exit(0);
})();
````

### コレクションの削除

コレクションの中のドキュメントをすべて削除すると、コレクションも削除されるみたいでした。  

何らかの問題があってドキュメントが削除できなかった場合は、Firebase の管理ダッシュボードから削除します。
