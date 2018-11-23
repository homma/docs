Title: can we update the $arch for aarch64 to have capability to choose arm64?

Currently, $arch for aarch64 is set as "armv7l".

https://github.com/hokaccha/nodebrew/blob/master/nodebrew
````perl
    } elsif ($machine =~ m/aarch64/) {
        $arch = 'armv7l';
````

The armv7l binaries provided by Node.js are 32-bit binary while aarch64 is a 64-bit archtecture.

````sh
$ file node-v11.2.0-linux-armv7l/bin/node 
node-v11.2.0-linux-armv7l/bin/node: ELF 32-bit LSB executable, ARM, EABI5 version 1 (GNU/Linux), dynamically linked, interpreter /lib/ld-linux-armhf.so.3, for GNU/Linux 3.16.42, not stripped
````

This is because the original pull-request is made for Chrome OS where the OS is 64-bit but the userland is 32-bit for some efficiency.

Below is the original pull-request to add an aarch64 configuration to the Nodebrew.

PR : https://github.com/hokaccha/nodebrew/pull/69  
Merge : https://github.com/hokaccha/nodebrew/commit/5117ab1d8d84e6c8595cc4437084f20614d0dcb6

And it seems that the request came from the following issue of Chromebrew for your reference.

https://github.com/skycocker/chromebrew/issues/2405

Since aarch64 is a 64-bit architecture, it would be nice if we can choose "arm64" along with "armv7l" as well.  
Node.js offers arm64 64-bit binaries which are native to aarch64.

````sh
$ file node-v11.2.0-linux-arm64/bin/node 
node-v11.2.0-linux-arm64/bin/node: ELF 64-bit LSB executable, ARM aarch64, version 1 (GNU/Linux), dynamically linked, interpreter /lib/ld-linux-aarch64.so.1, for GNU/Linux 3.7.0, BuildID[sha1]=327b8840de15505d13a9dc1ca8cfa3662ca6eb5b, not stripped
````

I need this to use Nodebrew on aarch64 systems where the userland is also 64-bit such as Debian 9 on Gemini PDA.
It may be the case for Raspberry Pi 64-bit OS as well.

Here's the patch.  

It uses `use64bitint` to check the bitness of the userland binary by picking up the perl itself as a sample.

I have tested it on a Debian 9 aarch64 system.  
It downloads and installs the 64-bit binary with no problem.

I have not tested it for Chrome OS since I do not have one.  
I hope it works well on the Chrome OS.

````diff
diff --git a/nodebrew b/nodebrew
index 9f6da59..f1d7620 100755
--- a/nodebrew
+++ b/nodebrew
@@ -733,7 +733,12 @@ sub system_info {
     } elsif ($machine =~ m/armv7l/) {
         $arch = 'armv7l';
     } elsif ($machine =~ m/aarch64/) {
-        $arch = 'armv7l';
+        use Config;
+        if(defined($Config{use64bitint})) {
+          $arch = 'arm64';
+        } else {
+          $arch = 'armv7l';
+        }
     } elsif ($sysname =~ m/sunos/i) {
         # SunOS $machine => 'i86pc'. but use 64bit kernel.
         # Solaris 11 not support 32bit kernel.
````

