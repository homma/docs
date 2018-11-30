# curl で Gist を操作する

## Anonymous ユーザとして Gist に投稿する

### Gist に投稿する
- 認証なしで Gist にデータを投稿すると Anonymous ユーザでの投稿になります
- 自分のアカウントと全く関係なくデータを投稿できます

- Gist への投稿は HTTP の POST メソッドを使用します
- curl の -d オプションでデータを指定すると自動的に POST メソッドが使用されます

- 指定するデータは、JSON 形式で、{"description":..., "public": true or false, "files": {"file name": {"content": "text data"}}} のようになります
- description には説明を記述します
- public には公開 Gist にするか、秘密 Gist にするかを指定します
- 秘密 Gist にしても閲覧制限が発生するわけではなく、URL を知っている人は誰でもアクセス可能です
- "file name" の箇所には、ファイル名を記述します
- 一つの Gist に複数のファイルを格納することが可能です
- content に Gist に投稿するデータを記述します

- データの投稿先は https://api.github.com/gists です

- Gist への投稿
````
$ curl -L -d '{"description":"test","public":false,"files":{"data":{"content":"my gist entry!"}}}' https://api.github.com/gists
````

### 投稿した Gist の URL を調べる
- Gist に投稿すると、処理結果が返されます
- 処理結果の中の、files.data.raw_url に Gist の生データの場所が含まれています

- Gist に投稿し、データの場所を取得する
````
$ curl -s -L --data '{"description":"test","public":false,"files":{"data":{"content":"my gist entry!"}}}' https://api.github.com/gists | \
ruby -r JSON -e 'data = JSON.parse(STDIN.read()); puts data["files"]["data"]["raw_url"]'
````

### Gist に投稿するデータを変数で定義する

- 変数化して操作しやすくします
````
$ DESCRIPTION="test"
$ FILENAME="data"
$ CONTENTS="$(date +%Y-%m-%dT%H:%M:%S): foo-bar-baz-quux"
$ curl -s -L --data '{"description":"'${DESCRIPTION}'","public":false,"files":{"'${FILENAME}'":{"content":"'"${CONTENTS}"'"}}}' https://api.github.com/gists | \
ruby -r JSON -e 'data = JSON.parse(STDIN.read()); puts data["files"]["'${FILENAME}'"]["raw_url"]'
````

### Anonymous Gist の注意点
- 一度投稿した Gist のデータは編集や削除ができません

## 自分のアカウント内の Gist を操作する（パスワードを使用したアクセス）

### Gist の投稿
- curl -H <認証情報> -d <投稿するデータ> https://api.github.com/gists を実行すると Gist にデータを投稿できます

````bash
$ AUTH=$(echo -n "user:passwd" | base64)
$ HEADER="Authorization: Basic ${AUTH}"
$ DATA='{"description":"test","public":true,"files":{"data":{"content":"my gist post"}}}'
$ GIST=$(curl -sH "${HEADER}" -d "${DATA}" https://api.github.com/gists)
$ ID=$(echo ${GIST} | node -e 'process.stdin.on("data", d => console.log(JSON.parse(d).id))')
$ echo ${ID}
````
- ID が Gist のユニーク ID
- メソッドは POST

### 投稿済みの Gist 一覧の取得
- https://api.github.com/gists を GET すると Gist の一覧を取得できます
- 認証情報をつけないで GET すると全ての Gist が対象となります

````
$ AUTH=$(echo -n "user:passwd" | base64)
$ HEADER="Authorization: Basic ${AUTH}"
$ curl -H "${HEADER}" https://api.github.com/gists
````

### Gist の検索
- description で検索する
````bash
$ GISTS=$(curl -sH "${HEADER}" https://api.github.com/gists)
$ DESCRIPTION="test post"
$ ID=$(echo ${GISTS} |\
node -e 'process.stdin.on("data", d => {let j = JSON.parse(d); console.log(j.filter(v => v.description == process.argv[1] )[0].id)})' "${DESCRIPTION}")
$ echo ${ID}
````

### Gist の更新（データの更新）
````
$ ID=<アップロードしたデータの ID>
$ DATA='{"description":"test","files":{"data":{"content":"this is my update."}}}' 
$ curl -H "${HEADER}" -d "${DATA}" https://api.github.com/gists/${ID}
````

- 同じ ID で POST すると上書きされる
- -X PATCH でも更新できる

### Gist の更新（ファイルの追加）
````
$ ID=<アップロードしたデータの ID>
$ DATA='{"description":"test","files":{"data2":{"content":"my file 2."}}}' 
$ curl -H "${HEADER}" -d "${DATA}" https://api.github.com/gists/${ID}
````

- 同じ ID に対して、異なるファイル名を POST するとファイルが追加される

### Gist の削除
- 最新の Gist を削除する例
````
$ GISTS=$(curl -sH "${HEADER}" https://api.github.com/gists)
$ ID=$(echo ${GISTS} | node -e 'process.stdin.on("data", d => {let j = JSON.parse(d); console.log(j[0].id)})')
$ curl -X DELETE -H "${HEADER}" https://api.github.com/gists/${ID}
````

## 自分のアカウント内の Gist を操作する（トークンを使用したアクセス）
- OAuth 認証を使用したアクセス方法

### トークンの作成
- personal access token を作成します
- curl を使用するか https://github.com/settings/tokens で生成します
````
$ AUTH=$(echo -n "user:passwd" | base64)
$ HEADER="Authorization: Basic ${AUTH}"
$ DATA='{"scopes":["gist"], "note":"gist updates"}'
$ curl -H "${HEADER}" -d "${DATA}" https://api.github.com/authorizations
````

- Basic 認証を使ってトークンを作成するので、ユーザ名とパスワードを指定する必要があります

- ユーザ名とパスワードは URL に挿入することも可能
````
$ curl -d "${DATA}" https://user:passwd@api.github.com/authorizations
````

### トークンを使用して Gist にアクセスする
- Gist に投稿する
````bash
$ OAUTH_TOKEN=<OAUTH_TOKEN>
$ HEADER="Authorization: token ${OAUTH_TOKEN}"
$ DATA='{"description":"test","public":true,"files":{"data":{"content":"my gist post"}}}'
$ curl -H "${HEADER}" -d "${DATA}" https://api.github.com/gists
````

### 作成されたトークンを確認する
- トークン自体は表示されません
````
$ AUTH=$(echo -n "user:passwd" | base64)
$ HEADER="Authorization: Basic ${AUTH}"
$ TOKENS=$(curl -s -H "${HEADER}" https://api.github.com/authorizations)
$ echo ${TOKENS} | \
node -e 'process.stdin.on("data", d => {let j = JSON.parse(d);let arr = j.filter(v => v.app.name == "gist updates"); console.log(arr[0])})'
````

### トークンの一覧を確認する
- トークン自体は表示されません
````
$ AUTH=$(echo -n "user:passwd" | base64)
$ HEADER="Authorization: Basic ${AUTH}"
$ curl -sH "${HEADER}" https://api.github.com/authorizations
````

- この出力からトークンの ID を取得することができます

### トークンを削除する
````
$ ID=<トークンの ID>
$ curl -X DELETE -sH "${HEADER}" https://api.github.com/authorizations/${ID}
````

- トークンの ID は、トークンの一覧から取得できます
- https://github.com/settings/tokens からも削除できます

## ToDo
- JavaScript から操作する方法の確立
  - XMLHttpRequest で OK なはず
  - GitHub API は CORS をサポートしている（[参考](https://developer.github.com/v3/#cross-origin-resource-sharing)）

## 参考情報

### Gist API
- [API](https://developer.github.com/v3/gists/)
- [About gists](https://help.github.com/articles/about-gists/)

### 認証
- https://developer.github.com/v3/oauth/
- https://developer.github.com/v3/oauth_authorizations/#create-a-new-authorization
- https://help.github.com/articles/creating-a-personal-access-token-for-the-command-line/