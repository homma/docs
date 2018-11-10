## Android の外付けキーボードのレイアウトを変更する

### はじめに

`iClever IC-BK03` の `Caps Lock` キーを `Ctrl` キーに変更します。  
（`Caps Lock` は使用しないため、キーのスワップはしません。）

キーの変更には、Android 4.1 以降で使用可能な [User-installable keymaps](https://developer.android.com/about/versions/jelly-bean) を使用します。  
手順としては、カスタマイズした [Key Character Map Files](https://source.android.com/devices/input/key-character-map-files) (`KCM ファイル`) を作成し、それをアプリケーションにパッケージングして Android 端末にインストールします。  

[Keyboard Layout Files](https://source.android.com/devices/input/key-layout-files) (`KL ファイル`) のインストールには root 権限が必要なため、KL ファイルは直接設定せず、KCM ファイルで設定することにします。

追加で、別のキーに `LANGUAGE_SWITCH` と `ZENKAKU_HANKAKU` も割り当ててみます。  
上手くすれば `Termux` で `iWnn` が使用できるかもしれません。

### 環境

- Fire Tablet 7
- iClever IC-BK03
- Termux

Fire Tablet は以下のドキュメントを参照して、開発者オプションを有効にしてあります。

- https://developer.amazon.com/docs/fire-tablets/connecting-adb-to-device.html

### サンプルコード

以下のコードを参考にします。

- https://github.com/tialaramex/BritishKeyboard
- https://github.com/hanhan1978/mackeymap
- https://github.com/y10g/android_user-keymap_jp109keyboard
- https://github.com/8796n/GeminiKCM/tree/master/app/src/main/res/raw

### 参考手順

以下のページを参考にします。

- https://monoworks.co.jp/post/2018-09-17-chnage-gemini-keymap/
- https://blog.8796.jp/8796kanri/2018/06/gemini-pda用user-installable-keymapsのまとめ.html
- http://ayati.cocolog-nifty.com/blog/2018/06/gemini-pdaroot-.html

キーボードの `Vendor ID` と `Product ID` の確認方法は以下のページを参考にします。

- http://www.cory.jp/android/kbd_hid.html

### Vendor ID と Product ID を取得する

`Termux` から `cat /proc/bus/input/devices` コマンドを実行して確認します。  
結果は以下の通りでした。

````
Bus=0005
Vendor=04e8
Product=7021
Version=001b
Name="iClever IC-BK03 Keyboard"
````

### デフォルトの Keylayout と Keychars を確認する

デフォルトの `Generic.kl` ファイルと `Generic.kcm` ファイルは以下にあります。

- [Generic.kl](https://android.googlesource.com/platform/frameworks/base/+/master/data/keyboards/Generic.kl)
- [Generic.kcm](https://android.googlesource.com/platform/frameworks/base/+/master/data/keyboards/Generic.kcm)

端末内では以下の場所に配置されています。

- /system/user/keylayout/Generic.kl
- /system/user/keychars/Generic.kcm

### 設定を決める

`Generic.kl` ファイルと `Generic.kcm` ファイルを見て設定を決めます。

KCM ファイルの書き方は以下のドキュメントを参考にします。

- https://source.android.com/devices/input/key-character-map-files

`CAPS_LOCK` は `key 58` なので、これを `CTRL_LEFT` に上書きします。  

`ALT_RIGHT` は `key 100` なので、これを `ZENKAKU_HANKAKU` に割り当てます。  
`META_RIGHT` (`key 126`) も `LANGUAGE_SWITCH` に割り当てます。

`Ctrl-Space` は、デフォルトでは `fallback LANGUAGE_SWITCH` になっていますが、`fallback` を外します。

設定はこんな感じになりそうです。

````
type FULL

# CAPS_LOCK
map key 58 CTRL_LEFT

# ALT_RIGHT
map key 100 ZENKAKU_HANKAKU

# META_RIGHT
map key 126 LANGUAGE_SWITCH

# Ctrl-Space to always LANGUAGE_SWITCH
key SPACE {
    label:                              ' '
    base:                               ' '
    alt, meta:                          fallback SEARCH
    ctrl:                               LANGUAGE_SWITCH
}
````

### アプリケーションのファイルを実装する

必要なファイルは以下のようです。  
`src` ディレクトリは必要になったら作成します。

````
AndroidManifest.xml
res/raw/caps_as_ctrl_plus_ime.kcm
res/values/strings.xml
res/xml/keyboard_layouts.xml
````

#### AndroidManifest.xm

`AndroidManifest.xml` には以下のドキュメントにある `receiver` を記述します。

- https://developer.android.com/reference/android/hardware/input/InputManager
````
 <receiver android:name=".InputDeviceReceiver"
         android:label="@string/keyboard_layouts_label">
     <intent-filter>
         <action android:name="android.hardware.input.action.QUERY_KEYBOARD_LAYOUTS" />
     </intent-filter>
     <meta-data android:name="android.hardware.input.metadata.KEYBOARD_LAYOUTS"
             android:resource="@xml/keyboard_layouts" />
 </receiver>
````

`@xml/keyboard_layouts` で `res/xml/keyboard_layouts.xml` ファイルを参照しています（多分）。

実際には、上記の XML 定義を `manifest` 宣言の中に記述する形になります。

- https://developer.android.com/guide/topics/manifest/manifest-element

#### caps_as_ctrl_plus_ime.kcm

KCM ファイルには先ほど作成した設定を記述します。

````
type FULL

# CAPS_LOCK
map key 58 CTRL_LEFT

# ALT_RIGHT
map key 100 ZENKAKU_HANKAKU

# META_RIGHT
map key 126 LANGUAGE_SWITCH

# Ctrl-Space to always LANGUAGE_SWITCH
key SPACE {
    label:                              ' '
    base:                               ' '
    alt, meta:                          fallback SEARCH
    ctrl:                               LANGUAGE_SWITCH
}
````

#### strings.xml

`app_name`、`keyboard_layouts_label`、`keyboard_layout_japanese_label` を設定します。

````xml
<?xml version="1.0" encoding="utf-8"?>
<resources xmlns:xliff="urn:oasis:names:tc:xliff:document:1.2">
    <string name="app_label">Caps as Ctrl plus IME Layout</string>
    <string name="keyboard_layout_label">Caps as Ctrl plus IME Layout</string>
    <string name="keyboard_layout_japanese_label">Japanese</string>
</resources>
````

#### keyboard_layouts.xml

`AndroidManifest.xml` ファイルで指定したファイルです。

マニュアルの以下の記述を参考にして記載します。

- https://developer.android.com/reference/android/hardware/input/InputManager
````
 <?xml version="1.0" encoding="utf-8"?>
 <keyboard-layouts xmlns:android="http://schemas.android.com/apk/res/android">
     <keyboard-layout android:name="keyboard_layout_english_us"
             android:label="@string/keyboard_layout_english_us_label"
             android:keyboardLayout="@raw/keyboard_layout_english_us" />
 </keyboard-layouts>
````

`caps_as_ctrl_plus_ime.kcm` および `strings.xml` への参照を設定します。

````xml
<?xml version="1.0" encoding="utf-8"?>
<keyboard-layouts xmlns:android="http://schemas.android.com/apk/res/android">
     <keyboard-layout android:name="keyboard_layout_japanese"
             android:label="@string/keyboard_layout_japanese_label"
             android:keyboardLayout="@raw/caps_as_ctrl_plus_ime" />
</keyboard-layouts>
````

### パッケージを作成する

Android Studio で APK ファイルを作成します。
