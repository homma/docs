---
title: Swift で音楽ファイルを再生するプログラムを作成する
status: draft
author: homma
---

--------------------------------------------------------------------------------

## Swift で利用可能なライブラリ

### NSSound
- https://developer.apple.com/documentation/appkit/nssound

### AVAudioPlayer
- https://developer.apple.com/documentation/avfaudio/avaudioplayer

--------------------------------------------------------------------------------

## NSSound を使用した最小限の音楽ファイル再生プログラム

````swift
import AppKit

let file = "/System/Library/Sounds/Ping.aiff"

guard let sound = NSSound.init(contentsOfFile: file, byReference: true)
else {
  print("file not found.")
  exit(1)
}

sound.play()

Thread.sleep(forTimeInterval: sound.duration)
````

--------------------------------------------------------------------------------

## NSSound で任意の音声ファイルを再生する

### main.swift

````swift
import AppKit

let args = CommandLine.arguments

guard args.count == 2 else {
  print("Usage: ./main <FILE>")
  exit(1)
}

let file = args[1]

guard let sound = NSSound.init(contentsOfFile: file, byReference: true)
else {
  print("please specify a music file.")
  exit(1)
}

sound.play()

Thread.sleep(forTimeInterval: sound.duration)
````

### ビルドと実行

````sh
$ swift main.swift
$ ./main /System/Library/Sounds/Ping.aiff
````

--------------------------------------------------------------------------------

## curses を使用して機能を追加する

ncurses を使って、中断・再開・早送り・早戻し・終了を実行できるようにしました

### main.swift
````swift
import AppKit
import Darwin.ncurses

let args = CommandLine.arguments

guard args.count == 2 else {
  print("Usage: ./main <FILE>")
  exit(1)
}

let file = args[1]

guard let sound = NSSound.init(contentsOfFile: file, byReference: true)
else {
  print("please specify a music file.")
  exit(1)
}

sound.play()

initscr()
cbreak()
noecho()

addstr("q: quit, p: pause, r: resume, b: back, f: forward ")

while true {
  let ch = UnicodeScalar(UInt8(getch()))
  switch ch {
  case "q":
    endwin()
    print("quit.")
    sound.stop()
    exit(0)
  case "p":
    sound.pause()
  case "r":
    sound.resume()
  case "b":
    sound.currentTime = (sound.currentTime < 10) ? 0 : sound.currentTime - 10
  case "f":
    sound.currentTime = sound.currentTime + 10
  default:
    break
  }
}
````

### ビルドと実行

````sh
$ swift main.swift
$ ./main /System/Library/Sounds/Ping.aiff
````

--------------------------------------------------------------------------------
