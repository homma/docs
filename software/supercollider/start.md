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

### コマンドライン開始

コマンドラインの操作に慣れているなら、GUI を起動するよりも `sclang` コマンドを使用するのが簡単です  

````sh
# sclang コマンドの実行
$ <インストールしたディレクトリ>/SuperCollider.app/Contents/MacOS/sclang 
````

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

### サーバ起動

サーバを起動することで音を再生することができるようになります

````
> s.boot
````

### サーバ停止

サーバを停止することで、再生中の音を停止できます

````
> s.quit
````

`<Command+.>` （コマンド＋ピリオド）や `<Ctrl+C>` でも停止可能です  

### サーバ再起動

サーバの再起動も可能です

````
> s.reboot
````

### ヘルプ表示

ヘルプ画面を呼び出して、使用可能なコマンドなどを確認できます
サーバのヘルプは `s.help` を実行するとヘルプ画面のウィンドウが開きます

````
> s.help
````

### コマンドライン終了

`<Ctrl-D>` でコマンドラインが終了します

## 音を鳴らす

### 単音を鳴らす

````
// 音を出力（デフォルトはドの音）
> ().play
````

````
// 音の大きさを 1 にして出力（デフォルトは 0.1）
> (amp: 1).play
````

````
// 周波数 (freq) を指定して出力
//
// ラ (440Hz)
> (freq: 440, amp: 1).play
````

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

`midicps` で MIDI ノート番号を周波数に変換できます  
`cps` は `Cycles Per Second` です

````
// MIDI ノート番号 (midinote) を周波数 (freq) に変換して出力
// https://doc.sccode.org/Classes/AbstractFunction.html
//
// ラ (69)
> (freq: 69.midicps).play
````

グローバル変数に格納してから音を出力することも可能です  
グローバル変数には一文字のアルファベットが使用できます

````
// グローバル変数に音を格納
//
// レ
> n = (note: 2, amp: 1)
> n.play
````

環境変数に格納してから音を出力することも可能です  
環境変数の名前は `~` で始まる文字列を指定できます

````
// 環境変数に音を格納
//
// ラ
> ~la = (freq: 440, amp: 1)
> ~la.play
````

### 和音を鳴らす

````
// ド・ミ・ソ (note)
> (note: [0,4,7]).play
````

````
// ド・ミ・ソ (degree)
> (degree: [0,2,4]).play
````

````
// ド・ミ・ソ (midinote)
> (midinote: [60,64,67]).play
````

````
// ド・ミ・ソ
> [0,4,7].do {|x| (note:x).play}
````

## 音を演奏する（順番に鳴らす）

````
// Pseq を使って順番に音を鳴らす
// ドミソ
> Pseq([(note:0), (note:4), (note:7), (note:[0,4,7])]).play
````

````
// 各音の長さを指定する
// ドミソ
// sustain: 音が鳴っている時間の長さ
// dur: 全体の時間の長さ
> ~music = [(note:0, sustain: 0.1, dur: 0.5), (note:4, sustain: 0.1, dur: 0.5), (note:7, sustain: 0.1, dur: 0.5), (note:[0,4,7], sustain: 1)]
> Pseq(~music).play
````

````
// Pbind を使ってより簡潔に記述
// ドレミ
> Pbind('degree',Pseq([0,1,2,3,4,5,6,7])).play
````

````
// 音の大きさ、長さ、間隔を変更する
// Pbind を使ってより簡潔に記述
// ドレミ
// 音の大きさ (amp): 1
// 音の長さ (sustain): 0.1
// 音の間隔 (dur): 0.1
> Pbind('degree',Pseq([0,1,2,3,4,5,6,7]), 'amp',1, 'sustain',0.1, 'dur', 0.1).play
````

````
// Pbind の結果を表示する
> p = Pbind('degree',Pseq([0,1,2,3,4,5,6,7]), 'amp',1, 'sustain',0.1, 'dur', 0.1)
> s = p.asStream
> while { n = s.next(()); n != nil } { n.postln }
````

## () は何なのか

() は `Event` データです

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

