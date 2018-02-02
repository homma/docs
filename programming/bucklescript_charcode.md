## BuckleScript string encoding problem : emoji example

This BuckleScript expression ...
````ocaml
Js.log "🍣";; (* This does not work. *)
````

is translated into the following JavaScript code.
````javascript
console.log("\xf0\x9f\x8d\xa3");
````

This JavaScript code does not display Sushi as expected.

The original string is encoded in UTF-8.  
But BuckleScript cannot handle UTF-8 string properly.  
Then the string is passed from BuckleScript to JavaScript as an array of 8bit data.

On the other hand, JavaScript requires UTF-16 (or UCS-2) encoding for a string.  
Therefore, the string that is an 8bit data array representing UTF-8 string is not properly rendered in web browsers.

You can resurrect the original string from the garbled string as below.
````javascript
const str = "\xf0\x9f\x8d\xa3" // BuckleScript String Data
const toUTF16 = str => {
  const arr = new Uint8Array(Array.from(str).map(x => x.charCodeAt(0)));
  const decoder = new TextDecoder('utf-8');
  return decoder.decode(arr);
}
toUTF16(str);
🍣
````

Or in Node.js:
````javascript
> const str = "\xf0\x9f\x8d\xa3"
> const toUTF16 = str => Buffer.from(str, 'ascii').toString('utf8');
> toUTF16(str);
'🍣'
````

Newly created HTML files are encouraged to use UTF-8 as its encoding.  
JavaScript uses UTF-16 as its String encoding.  

I hope BuckleScript handles this difference propery.
