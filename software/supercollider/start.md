---
title: SuperCollider の始め方
status: draft
author: homma
---

## リファレンス
- https://doc.sccode.org/Reference/Literals.html
- https://works.bepress.com/bruno-ruviaro/3/
- https://cs.wellesley.edu/~cs203/lecture_materials/sclang_basics/sclang_basics.pdf
- https://doc.sccode.org/Tutorials/Getting-Started/00-Getting-Started-With-SC.html
- https://doc.sccode.org/Tutorials/A-Practical-Guide/PG_01_Introduction.html

## インストール

以下のリンクからダウンロードしてインストールします

- https://supercollider.github.io/downloads

## 基本操作

### コマンドライン起動

コマンドラインの操作に慣れているなら、GUI を起動するよりも `sclang` コマンドを使用するのが簡単です  
macOS の場合は、以下の場所にコマンドがあります  

````sh
# sclang コマンドの実行
$ <インストールしたディレクトリ>/SuperCollider.app/Contents/MacOS/sclang 
````

コマンドに `PATH` を通しておきます

````sh
# パスを通す
$ PATH=<インストールしたディレクトリ>/SuperCollider/SuperCollider.app/Contents/MacOS:${PATH}
````

### コマンドの入力

プロンプト (`sc3>`) に対して文字列を入力することでコマンドを実行できます

````
sc3> "hello, world!"
-> hello, world!
````

以降はプロンプトを `>` と記載します

### サーバ起動

`SuperCollider` はサーバ・クライアント構成になっているため、まずサーバを起動する必要があります  
サーバを起動することで音を再生することができるようになります

````
> s.boot
````

### サーバ停止

サーバを停止することで、再生中の音を停止できます  
サーバを停止するコマンドは以下の通りです  

````
> s.quit
````

サーバは `<Command+.>` （コマンド＋ピリオド）や `<Ctrl+C>` でも停止可能です  

### サーバ再起動

サーバの再起動も可能です  
音の再生の停止とサーバの起動を一度に実行できます

````
> s.reboot
````

### ヘルプ表示

ヘルプ画面を呼び出して、使用可能なコマンドなどを確認できます  
サーバのヘルプは `s.help` を実行するとヘルプ画面のウィンドウが開きます

````
> s.help
````

サーバのヘルプは以下の URL からも参照できます

- https://doc.sccode.org/Classes/Server.html

### コマンドライン終了

`0.exit` でコマンドラインを終了させることができます

````
> 0.exit
````

`<Ctrl-D>` でもコマンドラインが終了します

## 音を鳴らす

### 単音を鳴らす

`().play` をコマンドラインに入力すると `ド` の音を鳴らすことができます

````
// 音を出力（デフォルトはドの音）
> ().play
````

音の大きさが小さかった場合は、`(amp: 1)` で音が大きくなります

````
// 音の大きさを 1 にして出力（デフォルトは 0.1）
> (amp: 1).play
````

発生させる音を指定する方法として、周波数を使用する方法があります  
`440` を指定すると `ラ` の音が出ます

````
// 周波数 (freq) を指定して出力
//
// ラ (440Hz)
> (freq: 440, amp: 1).play
````

音をノート `note` で指定する方法もあります  
`(note: 0)` で `ド` の音が出ます  
1 オクターブ 12 音で、数字を一つ増やすごとに対応する音も変化します

````
// 音 (note) を指定して出力
//
// ドレミ (0 - 12)
> (note: 0).play
> (note: 2).play
> (note: 4).play
> (note: 5).play
> (note: 7).play
> (note: 9).play
> (note: 11).play
> (note: 12).play
````

より低い音を鳴らす場合は、マイナスで指定するか、後述の `midinote` で指定できます  

````
// シ
> (note: -1).play
````

12 音の代わりに 7 音で指定することも可能です  
7 音で指定する場合は `degree` を使用します

````
// 音度 (degree) を指定して出力
//
// ドレミ (0 - 7)
> (degree: 0).play
> (degree: 1).play
> (degree: 2).play
> (degree: 3).play
> (degree: 4).play
> (degree: 5).play
> (degree: 6).play
> (degree: 7).play
````

MIDI ノート番号 `midinote` で音を指定することも可能です  
MIDI ノート番号では、`60` が `note: 0` に対応します

````
// MIDI ノート番号 (midinote) を指定して出力
//
// ドレミ (60 - 72)
> (midinote: 60).play
> (midinote: 62).play
> (midinote: 64).play
> (midinote: 65).play
> (midinote: 67).play
> (midinote: 69).play
> (midinote: 71).play
> (midinote: 72).play
````

`60` は、1 オクターブ 12 音で、0 から開始して 5 オクターブ + 1 の値です

### 音と周波数の変換

`midicps` で MIDI ノート番号を周波数に変換できます  
`cps` は `Cycles Per Second` です

````
// MIDI ノート番号 (midinote) を周波数 (freq) に変換して出力
// https://doc.sccode.org/Classes/AbstractFunction.html
//
// ラ (69)
> (freq: 69.midicps).play
````

`midicps` の逆方向の変換は `cpsmidi` です

````
// 周波数 (freq) を MIDI ノート番号に変換して出力
> (midinote: 440.cpsmidi).play
````

`note` を `freq` に変換したい場合は、`60` を足して `midinote` にしてから `midicps` を実行します

````
// note から freq への変換
> (0 + 60).midicps
> (-1 + 60).midicps
> (9 + 60).midicps
````

### 変数

グローバル変数に格納してから音を出力することも可能です  
グローバル変数には一文字のアルファベットを使用できます

````
// グローバル変数 (n) に音を格納し出力
//
// レ
> n = (note: 2, amp: 1)
> n.play
````

環境変数に格納してから音を出力することも可能です  
環境変数の名前は `~` で始まる文字列を指定できます

````
// 環境変数 (~la) に音を格納し出力
//
// ラ
> ~la = (freq: 440, amp: 1)
> ~la.play
````

### 和音を鳴らす

`note:` に配列を渡すと複数の音を同時に鳴らすことができます  
それにより和音を作成できます

配列は `[]` で作成します  
https://doc.sccode.org/Classes/Array.html

````
// ド・ミ・ソ (note)
> (note: [0,4,7]).play
````

`degree` や `midinote` でも和音を奏でることができます

````
// ド・ミ・ソ (degree)
> (degree: [0,2,4]).play
````

````
// ド・ミ・ソ (midinote)
> (midinote: [60,64,67]).play
````

よりプログラム的に和音を作ることも可能です

````
// ド・ミ・ソ
> [0,4,7].do {|x| (note:x).play}
````

## 音を演奏する（順番に鳴らす）

`Pseq` を使用することで、音を順番に鳴らすことができます  
https://doc.sccode.org/Classes/Pseq.html

````
// Pseq を使って順番に音を鳴らす
// ドミソ
> Pseq([(note:0), (note:4), (note:7), (note:[0,4,7])]).play
````

`sustain` と `dur` を指定して、再生する音の長さを変化させることができます

````
// 各音の長さを指定する
// ドミソ
// sustain: 音が鳴っている時間の長さ
// dur: 全体の時間の長さ
> ~music = [(note: 0, sustain: 0.1, dur: 0.5), (note: 4, sustain: 0.1, dur: 0.5), (note: 7, sustain: 0.1, dur: 0.5), (note: [0,4,7], sustain: 1)]
> Pseq(~music).play
````

`Pbind` を使用するとよりコンパクトに記述することができます  
https://doc.sccode.org/Classes/Pbind.html

以下の例では `Pseq` に指定した配列の要素数と同じサイズのシークエンスが作成され、 `Pbind` によりそれぞれの `degree` に値が設定されます  

````
// Pbind を使ってより簡潔に記述
// ドレミ
> Pbind('degree',Pseq([0,1,2,3,4,5,6,7])).play
````

`Pbind` を展開した結果は以下のコマンドで確認できます

````
// Pbind の結果を表示する
> p = Pbind('degree',Pseq([0,1,2,3,4,5,6,7]))
> t = p.asStream
> while { n = t.next(()); n != nil } { n.postln }
( 'degree': 0 )
( 'degree': 1 )
( 'degree': 2 )
( 'degree': 3 )
( 'degree': 4 )
( 'degree': 5 )
( 'degree': 6 )
( 'degree': 7 )
-> nil
````

以下の例では、`degree` にのみ `Pseq` に指定した配列内の値を適用し、他のパラメータには固定の値を設定しています

````
// 音の大きさ、長さ、間隔を変更する
// Pbind を使ってより簡潔に記述
// ドレミ
// 音の大きさ (amp): 1
// 音の長さ (sustain): 0.1
// 音の間隔 (dur): 0.1
> Pbind('degree',Pseq([0,1,2,3,4,5,6,7]), 'amp',1, 'sustain',0.1, 'dur', 0.1).play
````

## () は何なのか

() は `Event` データです  
https://doc.sccode.org/Classes/Event.html

````
// () は Event
> ().class
-> Event
````

````
// () は Event
> ().species
-> Event
````

`Event` はアクションを実行する際に使用するパラメータの値をまとめたものです  
以下の例では、`freq` に `440` を設定したイベントを `play` で実行しています

````
> (freq: 440).play
````

`Event` データはキーと値のペアを格納するデータ型としても使用することができます

````
// () はキーと値を格納するオブジェクトとしても使用できます
> ().keys
-> Set[  ]
> (freq: 440).keys
-> Set[ freq ]
> (freq: 440).values
-> List[ 440 ]
````

`Event` データはスコープを作成するのにも使用されます  
スコープの中では `var` を使用してローカル変数を定義できます

````
// スコープを作成し、`var` で変数を定義
> (
  var msg = "hello";
  msg.postln;
)
// スコープの外では `var` で作成したローカル変数にアクセスできない
> msg;
````

`().help` で `Event` の詳細な使用方法を確認することができます

````
// リファレンスの参照
> ().help
````

## 周波数アナライザを表示する

GUI の画面に波形を表示することができます

````
> s.freqscope
````

## Pluck でギター音を鳴らす

- https://doc.sccode.org/Classes/Pluck.html

`Pluck` を使用してギター音を鳴らすことができます

````
> { Pluck.ar(WhiteNoise.ar(1.0),1.0,1/440,1/440,10.0,0.01) }.play
````

````
> SynthDef('guitar1', { |out, freq = 440, dur = 3.0|
  var in, trig, maxdelaytime, delaytime, decaytime, coef, mul, add, pluck;
  in = WhiteNoise.ar(1.0);
  trig = 1.0;
  maxdelaytime = freq.reciprocal;
  delaytime = freq.reciprocal;
  decaytime = dur;
  coef = 0.01;
  mul = 1.0;
  add = 1.0;
  pluck = Pluck.ar(in, trig, maxdelaytime, delaytime,
                   decaytime, coef, mul, add);
  Out.ar(out, pluck)
}).add
> (instrument: 'guitar1', freq: 60.midicps).play
````

## instrument を作成する

### 音色を作成する
````
> SynthDef('myinst1', {|out| Out.ar(out, SinOsc.ar(440, 0, 1))}).add
> p = (instrument: 'myinst1').play
> p.free
````

````
// freq を指定可能にする
> SynthDef('myinst2', {|out, freq = 440| Out.ar(out, SinOsc.ar(freq, 0, 1))}).add
> p = (instrument: 'myinst2', freq: 330).play
> p.free
> p = (instrument: 'myinst2', freq: 440).play
> p.free
````

### エンベロープを作成する
````
// エンベロープを設定して、時間の経過で停止する音を作成する
> SynthDef('myinst3', {|out, freq = 440|
    var sin = SinOsc.ar(freq, 0, 1);
    var env = EnvGen.kr(Env.perc, doneAction: Done.freeSelf);
    Out.ar(out, sin * env)}).add
> (instrument: 'myinst3').play
````

- https://doc.sccode.org/Classes/Env.html
- https://doc.sccode.org/Classes/Linen.html
- https://doc.sccode.org/Classes/Done.html
- https://doc.sccode.org/Classes/EnvGen.html

### default instrument で指定できるプロパティを確認する

````
> SynthDescLib.global['default']
-> SynthDesc 'default' 
Controls:
ControlName  P 0 out scalar 0.0
ControlName  P 1 freq control 440.0
ControlName  P 2 amp control 0.10000000149012
ControlName  P 3 pan control 0.0
ControlName  P 4 gate control 1.0
   O audio OffsetOut out 2

````

### システムに用意されている instrument の一覧を見る

````
> SynthDescLib.global.synthDescs.keys.do {|x| x.postln}
````

