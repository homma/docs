## Android の外付けキーボードのレイアウトを変更する

### はじめに

`iClever IC-BK03` の `Caps Lock` キーを `Ctrl` キーに変更します。  
（`Caps Lock` は使用しないため、キーのスワップはしません。）

キーの変更には、Android 4.1 以降で使用可能な [User-installable keymaps](https://developer.android.com/about/versions/jelly-bean) を使用します。  
手順としては、カスタマイズした [Key Character Map Files](https://source.android.com/devices/input/key-character-map-files) (`KCM ファイル`) を作成し、それをアプリケーションにパッケージングして Android 端末にインストールします。  

### メモ

[Keyboard Layout Files](https://source.android.com/devices/input/key-layout-files) (`KL ファイル`) のインストールには root 権限が必要なため、KL ファイルは直接設定せず、KCM ファイルで設定することにします。

`LANGUAGE_SWITCH` や `ZENKAKU_HANKAKU` を有効にすれば `Termux` で `iWnn` が使用できるかと思いましたが、動作しませんでした。  
`LANGUAGE_SWITCH` はキーボードのレイアウトを切り替える機能のようです。

### 環境

- Fire Tablet 7
- Fire OS 5.3.6.4
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

type は OVELAY にしました。

`CAPS_LOCK` は `key 58` なので、これを `CTRL_LEFT` に上書きします。  

````
type OVERLAY

# CAPS_LOCK
map key 58 CTRL_LEFT
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

res/raw/caps_as_ctrl_plus_ime.kcm:
````
type OVERLAY

# CAPS_LOCK
map key 58 CTRL_LEFT
````

#### strings.xml

`app_name`、`keyboard_layouts_label`、`keyboard_layout_japanese_label` を設定します。

res/values/strings.xml:
````xml
<?xml version="1.0" encoding="utf-8"?>
<resources xmlns:xliff="urn:oasis:names:tc:xliff:document:1.2">
    <string name="app_name">Caps as Ctrl plus IME Layout</string>
    <string name="keyboard_layouts_label">Caps as Ctrl plus IME Layout</string>
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

res/xml/keyboard_layouts.xml:
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

#### Android Studio のインストール

- ダウンロード
  - https://developer.android.com/studio/

- dmg ファイルをダブルクリック
- Android Studio.app を Applications フォルダにドラッグアンドドロップ
- Android Studio.app をダブルクリック

- `[Next]` をクリック
- `[Next]` をクリック
- `[Next]` をクリック
- `[Finish]` をクリック

`~/Library/Android/` 以下に色々とインストールされます。

- Intel HAXM installer に関する警告が出るので、「セキュリティとプライバシー」から実行を許可
- Intel HAXM はエミュレータの動作を高速化するためのもののようです。

- [Finish] をクリック

#### プロジェクトの作成

インストーラからの流れでプロジェクトを作成します。

- `Start a new Android Studio project` をクリック

設定は以下にしました。

````
Application Name : Caps as Ctrl plus IME Layout
Company domain : homma.github.com
````

- `[Next]` をクリック

`Fire OS 5` は `Android 5` ベースなので、`API 21` を選択します。

- `[Next]` をクリック
- `Add No Activity` をクリック
- `[Next]` をクリック
- `[Finish]` をクリック

#### AndroidManifest.xml の実装

`app >> src >> main >> AndroidManifest.xml` をダブルクリックします。

以下の部分を変更します。

````xml
    <application
        android:allowBackup="true"
        android:icon="@mipmap/ic_launcher"
        android:label="@string/app_name"
        android:roundIcon="@mipmap/ic_launcher_round"
        android:supportsRtl="true"
        android:theme="@style/AppTheme" />
````

以下のように変更します。

````
    <application
        android:allowBackup="true"
        android:icon="@mipmap/ic_launcher"
        android:label="@string/app_name"
        android:roundIcon="@mipmap/ic_launcher_round"
        android:supportsRtl="true"
        android:theme="@style/AppTheme">

 <receiver android:name=".InputDeviceReceiver"
         android:label="@string/keyboard_layouts_label">
     <intent-filter>
         <action android:name="android.hardware.input.action.QUERY_KEYBOARD_LAYOUTS" />
     </intent-filter>
     <meta-data android:name="android.hardware.input.metadata.KEYBOARD_LAYOUTS"
             android:resource="@xml/keyboard_layouts" />
 </receiver>

    </application>
````

#### caps_as_ctrl_plus_ime.kcm

`src >> main >> res` に `raw` ディレクトリを追加し、`caps_as_ctrl_plus_ime.kcm` ファイルを追加します。  
ファイルの中身に以下を記載します。

res/raw/caps_as_ctrl_plus_ime.kcm:
````
type FULL

# CAPS_LOCK
map key 58 CTRL_LEFT
````

#### strings.xml

`src >> main >> res >> values >> strings.xml` をダブルクリックします。

内容を以下のように変更します。

````xml
<resources xmlns:xliff="urn:oasis:names:tc:xliff:document:1.2">
    <string name="app_name">Caps as Ctrl plus IME Layout</string>
    <string name="keyboard_layouts_label">Caps as Ctrl plus IME Layout</string>
    <string name="keyboard_layout_japanese_label">Japanese</string>
</resources>
````

#### caps_as_ctrl_plus_ime.kcm

`src >> main >> res` に `xml` ディレクトリを追加し、`keyboard_layouts.xml` ファイルを追加します。  
ファイルの内容は以下を記載します。

res/xml/keyboard_layouts.xml:
````xml
<keyboard-layouts xmlns:android="http://schemas.android.com/apk/res/android">
     <keyboard-layout android:name="keyboard_layout_japanese"
             android:label="@string/keyboard_layout_japanese_label"
             android:keyboardLayout="@raw/caps_as_ctrl_plus_ime" />
</keyboard-layouts>
````

#### ビルド

金槌アイコンをクリックしてビルドを実行します。  
エラーが出なければ成功です。

#### パッケージの作成

メニューから `Build >> Build Bundle(s) / APK(s) >> Build APK(s)` を選択します。

パッケージのビルドが完了すると、`~/AndoridStudioProjects/CapsasCtrlplusIMELayout/app/build/outputs/apk/debug/app-debug.apk` が作成されます。

### パッケージの転送

あらかじめ Android 側で `開発者オプション` から `ADB を有効にする` をオンにしておきます。

- メニューから `Run >> Run` を選択
- `Edit Configuration..` をクリック
- `Launch Opitions` で `Launch:` に `Nothing` を指定
- `[Run]` をクリック
- `Connected Devices` で Android 端末が選択されていることを確認
- `[OK]` をクリック

### リリースビルドの作成

メニューから `Build >> Generate Signed Bundle / APK` を選択します。

- APK を選択
- `[Next]` をクリック
- `[Create new...]` をクリック
- `Key store path` にキーファイルを保存する場所を指定
- パスワードを設定
- Certificate の情報を設定
  - First and Last Name だけでも OK
- `[OK]` をクリック
- `Remember passwords` にチェックを入れる
- `[Next]` をクリック
- `Build Type` が `release` になっていることを確認
- `Signature Versions` に `V2` を選択　
- `[Finish]` をクリック

### リリースビルドのインストール

- メニューから `Run >> Run` を選択
- 以下のエラーが表示される

````
Error: The apk for your currently selected variant (app-release.apk) is not signed. Please specify a signing configuration for this variant (release).
````

- `[Fix]` をクリック
- `+` を押して、先ほど作成したキーファイルの設定を記入
- `Build Types` で `release` を選択
- `Signing config` に `config` と入力
- `[OK]` をクリック

## メモ

### Vendor ID と Product ID を取得する（未使用）
今回 `Vendor ID` と `Product ID` は使用しなかったため、この手順は実施しませんでした。

キーボードの `Vendor ID` と `Product ID` の確認方法は以下のページを参考にします。

- http://www.cory.jp/android/kbd_hid.html

KCM ファイルの名前に `Vendor ID` と `Product ID` を入れることで、自動判別してくれるみたい。

`Termux` から `cat /proc/bus/input/devices` コマンドを実行して確認します。  
結果は以下の通りでした。

````
Bus=0005
Vendor=04e8
Product=7021
Version=001b
Name="iClever IC-BK03 Keyboard"
````
