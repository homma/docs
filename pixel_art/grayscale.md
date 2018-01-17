グレイスケールは 256 色が標準的みたい。

## 資料
- [List of monochrome and RGB palettes](https://en.wikipedia.org/wiki/List_of_monochrome_and_RGB_palettes)
- [List of video game console palettes](https://en.wikipedia.org/wiki/List_of_video_game_console_palettes)

## ドット絵用グレースケールパターン
深い意味はありませんが、00 00 00 と ff ff ff は空けておきます。  
スプライトとして使用するときに、マスクしやすくなるかもしれないので。  

### 4 色グレイスケール
````rgb
08 08 08
58 58 58
a8 a8 a8
f8 f8 f8
````

````javascript
[0, 5, 10, 15].map(v => v * 16 + 8).map(v => v.toString(16)).forEach(v => console.log(v, v, v))
````

### 8 色グレイスケール
````rgb
10 10 10
30 30 30
50 50 50
70 70 70
90 90 90
b0 b0 b0
d0 d0 d0
f0 f0 f0
````

````javascript
a1 = Array(8).fill().map( (v, i) => i * 2 + 1 )
// [ 1, 3, 5, 7, 9, 11, 13, 15 ]
a1.map(v => (v * 16).toString(16)).forEach(v => console.log(v, v, v))
````
