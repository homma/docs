
Chrome OS は 64bit カーネルでユーザランドは 32bit みたい。  
Chrome OS を判別するルーチンが必要。

Title: can we update the $arch for aarch64 to arm64?

Currently, $arch for aarch64 is set as "armv7l".

https://github.com/hokaccha/nodebrew/blob/master/nodebrew
````perl
    } elsif ($machine =~ m/aarch64/) {
        $arch = 'armv7l';
````

And armv7l binaries provided by Node.js are 32-bit binary.

````sh
$ file node-v11.2.0-linux-armv7l/bin/node 
node-v11.2.0-linux-armv7l/bin/node: ELF 32-bit LSB executable, ARM, EABI5 version 1 (GNU/Linux), dynamically linked, interpreter /lib/ld-linux-armhf.so.3, for GNU/Linux 3.16.42, not stripped
````

Since aarch64 is a 64-bit architecture, it would be nice if it will use "arm64" instead.

````sh
$ file node-v11.2.0-linux-arm64/bin/node 
node-v11.2.0-linux-arm64/bin/node: ELF 64-bit LSB executable, ARM aarch64, version 1 (GNU/Linux), dynamically linked, interpreter /lib/ld-linux-aarch64.so.1, for GNU/Linux 3.7.0, BuildID[sha1]=327b8840de15505d13a9dc1ca8cfa3662ca6eb5b, not stripped
````

Here's the patch. I have tested it on Debian 9 aarch64.

````diff
diff --git a/nodebrew b/nodebrew
index 9f6da59..6a81d06 100755
--- a/nodebrew
+++ b/nodebrew
@@ -733,7 +733,7 @@ sub system_info {
     } elsif ($machine =~ m/armv7l/) {
         $arch = 'armv7l';
     } elsif ($machine =~ m/aarch64/) {
-        $arch = 'armv7l';
+        $arch = 'arm64';
     } elsif ($sysname =~ m/sunos/i) {
         # SunOS $machine => 'i86pc'. but use 64bit kernel.
         # Solaris 11 not support 32bit kernel.
````

Below is the original request to add an aarch64 configuratoin to the Nodebrew.

PR : https://github.com/hokaccha/nodebrew/pull/69
Merge : https://github.com/hokaccha/nodebrew/commit/5117ab1d8d84e6c8595cc4437084f20614d0dcb6

It seems that the request above came from the following issue for Chromebrew.

https://github.com/skycocker/chromebrew/issues/2405

// Since the other packages of Chromebrew use arm64, it may be safe for Nodebrew to use it as well.

https://github.com/skycocker/chromebrew/search?q=arm64&unscoped_q=arm64
