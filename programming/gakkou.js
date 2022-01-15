// https://twitter.com/nya3_neko2/status/1481503969965142016

const matrix = arr => {

  if(arr.length == 1) {
    return [arr[0]]
  }

  let result = [];
  arr.forEach((val, idx, arr) => {

    const rest = arr.slice(0, idx).concat(arr.slice(idx + 1));
    matrix(rest).forEach(x => result.push([val].concat(x)));

  })

  return result;

}

const str = "GAKKOU";
const res = matrix(str.split(''));

const res_str = res.map(x => x.join(''));
const res_set = new Set(res_str);
console.log([...res_set].sort()[99]);
