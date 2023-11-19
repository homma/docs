---
title: Swift で OpenGL のプログラムを作成する
status: draft
author: homma
---

--------------------------------------------------------------------------------

OpenGL と GLUT は modulemap が作成されており、デフォルトの状態で swift から利用可能です

````
/Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX.sdk/System/Library/Frameworks/OpenGL.framework/Versions/A/Modules/module.modulemap

/Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX.sdk/System/Library/Frameworks/GLUT.framework/Modules/module.modulemap
````

--------------------------------------------------------------------------------

## GLUT を使用した OpenGL プログラム

### main.swift

````swift
import GLUT

func update() {
  glClear(UInt32(GL_COLOR_BUFFER_BIT))

  glBegin(UInt32(GL_POLYGON))
  glColor3d(0.0, 0.0, 1.0)
  glVertex3d(0.0, 0.8, 0.0)
  glColor3d(0.0, 1.0, 0.0)
  glVertex3d(-0.8, -0.8, 0.0)
  glColor3d(1.0, 0.0, 0.0)
  glVertex3d(0.8, -0.8, 0.0)
  glEnd()

  glFlush()
}

func main() {
  var argc = CommandLine.argc
  let argv = CommandLine.unsafeArgv

  glutInit(&argc, argv)

  glutInitDisplayMode(UInt32(GLUT_RGBA))
  let _ = glutCreateWindow("My GLUT Window")
  glutDisplayFunc(update)
  glClearColor(1.0, 1.0, 1.0, 0.0)

  glutMainLoop()
}

main()
````

`import GLUT` とするだけで GLUT と OpenGL が使用可能になります  
`import OpenGL` は記載しなくても良いようです

`GL_POLYGON` などの定数マクロは `INT32` 型としてインポートされているようです  
`UInt32` に変換してあげる必要がありました  

### コンパイル

````sh
$ swiftc main.swift -framework OpenGL -Xcc -DGL_SILENCE_DEPRECATION 
````

`-framework` オプションで `OpenGL` を指定します  
`-Xcc -DGL_SILENCE_DEPRECATION` オプションで警告を抑制します  

--------------------------------------------------------------------------------
