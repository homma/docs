
`hdiutil create` コマンドで DMG ファイルを作成できる。

# ディレクトリからボリュームを作成
````sh
$ DIR="./foo"
$ VOL_NAME=myvol
$ FILE_NAME=myvol.dmg
$ hdiutil create -srcdir ${DIR} -volname ${VOL_NAME} ${FILE_NAME}
````

## パスワードをかけたい場合
````sh
$ DIR="./foo"
$ VOL_NAME=myvol
$ FILE_NAME=myvol.dmg
$ hdiutil create -encryption -stdinpass -srcdir ${DIR} -volname ${VOL_NAME} ${FILE_NAME}
````

# 書き込み可能な固定サイズのボリュームを作成
````sh
$ FS="HFS+"
$ SZ=100m
$ VOL_NAME=myvol
$ FILE_NAME=myvol.dmg
$ hdiutil create -fs ${FS} -size ${SZ} -volname ${VOL_NAME} ${FILE_NAME}
````

## パスワードをかけたい場合
````sh
$ FS="HFS+"
$ SZ=100m
$ VOL_NAME=myvol
$ FILE_NAME=myvol.dmg
$ hdiutil create -fs ${FS} -size ${SZ} -volname ${VOL_NAME} -encryption -stdinpass ${FILE_NAME}
````
