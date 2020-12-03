open Str;

let file = Stdlib.open_in("source");

let length = Stdlib.in_channel_length(file);
let str = Stdlib.really_input_string(file, length);

let lines = split(regexp("\n"), str);




let countLetters: (string, char) => int = (str, letter) => {
  List.length(String.split_on_char(letter, str)) - 1;
}

let result = List.fold_left((acc, line) => {
  let sections: list(string) = split(regexp(" "), line);
  let extremes = split(regexp("-"), Array.of_list(sections)[0]) |> List.map(int_of_string);
  let letter: char = String.get(Array.of_list(sections)[1], 0);
  let password = Array.of_list(sections)[2];
  let min = Array.of_list(extremes)[0];
  let max = Array.of_list(extremes)[1];
  let _numberOfLetter = countLetters(password, letter);
  let getFromPassword = String.get(password);
  // numberOfLetter >= min && numberOfLetter <= max ? acc + 1 : acc;
  (getFromPassword(min - 1) == letter && getFromPassword(max - 1) == letter)
    || (getFromPassword(min - 1) != letter && getFromPassword(max - 1) != letter)
    ? acc : acc + 1;
}, 0, lines);

print_endline(string_of_int(result));
