## Node.js misc scripts

### コマンドオプションの取得
- process.argv からアクセス可能

````javascript
//// checking arguments in node.js
// Usage: node ./args.js arg ...

const check_args = () => {

  n_args = process.argv.length;
  if(n_args != 3) {

    console.log("Usage: node args.js arg ...");
    process.exit();

  }
}

const get_first_arg = () => {

  return process.argv[2];

}

const main = () => {

  check_args();
  const arg = get_first_arg();
  console.log(arg);

}

main();
````
