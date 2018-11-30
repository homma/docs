### PR 手順

#### 事前に確認すること

1. 過去の PR を見て、PR のタイトルに書式があるか確認する
2. 過去の PR を見て、ブランチ名の命名法則があるか確認する (less important)

#### 事前に決めておくこと

1. branch name
2. commit message
3. title of the prull request
4. summary of the pull request

#### PR 手順
- 以下の手順に加えて、Fork してから時間が経っている場合は、PR を送る前に本家の変更を取り込んでおく必要がある

1. Fork する
2. ローカルに clone する

````sh
$ git clone https://github.com/...
````

3. branch を作成する

````sh
$ git checkout -b <branch name>
$ git status
````

4. 修正を加える

5. commit する

````sh
$ git add .
$ git commint -m <commit message>
````

6. branch を push する

````sh
$ git push origin <branch name>
````

7. PR を作成する