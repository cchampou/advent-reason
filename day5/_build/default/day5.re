open Str;

let file = Stdlib.open_in("source");

let length = Stdlib.in_channel_length(file);
let str = Stdlib.really_input_string(file, length);

let lines: list(string) = split(regexp("\n"), str);

let getRow = str => {
  let chars = Base.String.to_list(str);
  List.fold_left(((min, max, l, r), x) => {
    switch (x) {
      | 'F' => (min, min + (max - min) / 2, l, r)
      | 'B' => (min + (max - min + 1) / 2, max, l, r)
      | 'R' => (min, max, r - (r - l) / 2, r)
      | 'L' => (min, max, l, l + (r - l) / 2)
      | _ => (min, max, l, r)
    }
  }, (0, 127, 0, 7), chars);
}

let results = lines
  |> List.map(getRow)
  |> List.map(((row, _, seat, _)) => row * 8 + seat)
  |> List.sort((a, b) => a - b);

let max = results
  |> List.fold_left((acc, x) => x > acc ? x : acc, 0);

let mySeat = results
  |> List.fold_left((acc, x) => {
    if (acc == 0) {
      x + 1;
    } else if (x == acc) {
      x + 1;
    } else {
      acc;
    }
  }, 0);

List.map(x => print_endline(x |> string_of_int), results);

mySeat |> string_of_int |> print_endline;
