
open Str;

let file = Stdlib.open_in("source");

let length = Stdlib.in_channel_length(file);
let str = Stdlib.really_input_string(file, length);

let lines: list(string) = split(regexp("\n"), str);

type follow = {
  line: int,
  hit: int,
};

let init = {
  line: 0,
  hit: 0,
};

let process = (x, y) => {
  let lineLength = String.length(Array.of_list(lines)[0]);
  List.fold_left((acc, line: string) => {
    let newCoordX = acc.line / y * x;
    if (acc.line mod y == 0) {
      switch(String.get(line, newCoordX mod lineLength)) {
        | '#' => { hit: acc.hit + 1, line: acc.line + 1 }
        | _ => { hit: acc.hit, line: acc.line + 1 }
      }
    } else {
      { hit: acc.hit, line: acc.line + 1 };
    }
  }, init, lines);
}

let results = [
  process(1, 1),
  process(3, 1),
  process(5, 1),
  process(7, 1),
  process(1, 2),
];

let resultint = List.fold_left((acc, x) => {
  acc * x.hit;
}, 1, results);

print_endline(string_of_int(resultint));
