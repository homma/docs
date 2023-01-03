// https://news.ycombinator.com/item?id=34224456

const reindeers = [];

const Reindeer = function(name, before, after) {
  this.name = name;
  this.before = before || [];
  this.after = after || [];

  reindeers.push(this);
};

Reindeer.prototype.add_after = function() {
  const args = [...arguments];
  args.forEach(elem => {
    if (!this.after.includes(elem)) {
      this.after.push(elem);
    }
  });
  args.forEach(elem => {
    if (!elem.before.includes(this)) {
      elem.before.push(this);
    }
  });
};

Reindeer.prototype.add_before = function() {
  const args = [...arguments];
  args.forEach(elem => {
    if (!this.before.includes(elem)) {
      this.before.push(elem);
    }
  });
  args.forEach(elem => {
    if (!elem.after.includes(this)) {
      elem.after.push(this);
    }
  });
};

Reindeer.prototype.p = function() {
  console.log(this);
};

const print_reindeers = reindeers => {
  console.log(reindeers);
  reindeers.forEach(elem => elem.p());
  reindeers.forEach(e => console.log(e.name));
};

const print_reindeers_name = reindeers => {
  reindeers.forEach(e => console.log(e.name));
};

//// vixen
const vixen = new Reindeer("vixen");
const rudolph = new Reindeer("rudolph");
const prancer = new Reindeer("prancer");
const dasher = new Reindeer("dasher");
const dancer = new Reindeer("dancer");
const comet = new Reindeer("comet");
vixen.add_before(rudolph, prancer, dasher);
vixen.add_after(dancer, comet);

//// dancer
const blitzen = new Reindeer("blitzen");
const donder = new Reindeer("donder");
dancer.add_before(donder, blitzen, rudolph);

//// comet
const cupid = new Reindeer("cupid");
comet.add_before(cupid, prancer, rudolph);

//// donder
donder.add_before(comet, vixen, dasher, prancer, cupid);

//// cupid
cupid.add_after(comet, blitzen, vixen, dancer, rudolph);

//// prancer
prancer.add_after(blitzen, donder, cupid);

//// blitzen
blitzen.add_before(cupid);
blitzen.add_after(dancer, vixen, donder);

//// rudolph
rudolph.add_before(prancer);
rudolph.add_after(dasher, dancer, donder);

//// dasher
dasher.add_before(prancer);
dasher.add_after(blitzen, dancer, vixen);

const reorder = (reindeers, reindeer) => {
  // console.log("b---");
  // print_reindeers_name(reindeers);

  // let ordered = false;

  //// divide list
  // [...before], reindeer, [...after]
  const idx = reindeers.indexOf(reindeer);
  let before = reindeers.slice(0, idx);
  let after = reindeers.slice(idx + 1);
  const b_len = before.length;
  const a_len = after.length;

  // console.log('idx')
  // console.log(idx)
  // console.log('1---')
  // print_reindeers_name(before)
  // console.log('2---')
  // print_reindeers_name(after)

  //// before list
  // add elements from reindeer.before to before if not yet included
  reindeer.before.forEach(elem => {
    if (!before.includes(elem)) {
      before.push(elem);
    }
  });

  // if (b_len != before.length) ordered = true;

  //// before list
  // remove elements of reindeer.after from before if included
  before = before.filter(elem => !reindeer.after.includes(elem));

  // if (b_len != before.length) ordered = true;

  //// after list
  // add elements from reindeer.after to after if not yet included
  reindeer.after.forEach(elem => {
    if (!after.includes(elem)) {
      after.push(elem);
    }
  });

  // if (a_len != after.length) ordered = true;

  //// after list
  // remove elements of reindeer.before from after if included
  after = after.filter(elem => !reindeer.before.includes(elem));

  // if (a_len != after.length) ordered = true;

  //// result list
  // concatinate all
  const ordered_list = [...before, reindeer, ...after];

  // console.log("a---");
  // print_reindeers_name(ordered_list);

  return ordered_list;
};

let lineup = Array.from(reindeers);

// 4 times loop resolves the quiz
// should check if the array has been chenged for each loop
for (let i = 0; i < 5; i++) {
  let arr = Array.from(lineup);

  arr.forEach(elem => {
    lineup = reorder(lineup, elem);
  });

  console.log(`${i} ----`);
  print_reindeers_name(lineup);
}
