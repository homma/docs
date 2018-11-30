
## Cloud Firestore の概要

Firestore は 1GB まで無料で使用可能な NoSQL データストア。  
無料利用枠を超えると使用できなくなる（超過分が課金されるのではなく）みたいなので、使いやすい。

JavaScript の SDK は以下にある。

- https://github.com/firebase/firebase-js-sdk/tree/master/packages/firebase

ウェブページから Firewstore にアクセスする際は、以下のファイルをインクルードする。

````html
<script src="https://www.gstatic.com/firebasejs/${FIREBASE_VERSION}/firebase-app.js"></script>
<script src="https://www.gstatic.com/firebasejs/${FIREBASE_VERSION}/firebase-firestore.js"></script>
````

Node.js から Firestore にアクセスする際は、`firebase` パッケージを導入する。

````sh
$ npm init
$ npm install --save firebase
````

ES6 modules を使用する場合は、以下のコードでモジュールをロードする。  
`import * as firetore ...` だと上手くロードできなかった。

````js
import firestore from 'firebase/app';
import 'firebase/database';
````

アクセスには以下の情報が必要。  
実質、ウェブ API キーと、プロジェクト ID があれば良い。

````js
const config = {
  apiKey: "<API_KEY>",
  authDomain: "<PROJECT_ID>.firebaseapp.com",
  projectId: "<PROJECT_ID>"
};
````

#### 参考
- https://github.com/firebase/firebase-js-sdk/tree/master/packages/firebase
- https://firebase.google.com/docs/firestore/quickstart?hl=ja
- https://firebase.google.com/docs/web/setup?hl=ja