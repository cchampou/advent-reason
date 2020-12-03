open Str;

let file = Stdlib.open_in("source");

let length = Stdlib.in_channel_length(file);
let str = Stdlib.really_input_string(file, length);

let numbersAsString = split(regexp("\n"), str);

let numbers = List.map(int_of_string, numbersAsString);

let process = (nums) => {
  List.iter((x) => {
    List.iter((y) => {
       List.iter((z) => {
          if (x + y + z == 2020) {
            print_endline(string_of_int(x * y * z));
          }
       }, nums);
    }, nums);
  }, nums);
};


process(numbers);

