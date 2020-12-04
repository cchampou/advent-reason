open Str;

let file = Stdlib.open_in("source");

let length = Stdlib.in_channel_length(file);
let str = Stdlib.really_input_string(file, length);

let passports: list(string) = split(regexp("\n\n"), str);

type byr = option(int);
type iyr = option(int);
type eyr = option(int);
type hgt = option(string);
type hcl = option(string);
type ecl = option(string);
type pid = option(string);
type cid = option(string);

type passport = {
  byr: byr,
  iyr: iyr,
  eyr: eyr,
  hgt: hgt,
  hcl: hcl,
  ecl: ecl,
  pid: pid,
  cid: cid,
};

let validateByr: string => byr = str => {
  let intVal = int_of_string(str);
  intVal >= 1920 && intVal <= 2002 ? Some(intVal) : None;
}

let validateIyr: string => iyr = str => {
  let intVal = int_of_string(str);
  intVal >= 2010 && intVal <= 2020 ? Some(intVal) : None;
}

let validateEyr: string => eyr = str => {
  let intVal = int_of_string(str);
  intVal >= 2020 && intVal <= 2030 ? Some(intVal) : None;
}

let validateHgt: string => hgt = str => {
  let unit = Str.last_chars(str, 2);
  let value = Str.string_before(str, String.length(str) - 2);
  if (String.length(value) > 0 && String.length(unit) == 2) {
    let intValue = int_of_string(value);
    switch ((unit, intValue)) {
      | ("cm", cmVal) => cmVal >= 150 && cmVal <= 193 ? Some(str) : None
      | ("in", inVal) => inVal >= 59 && inVal <= 76 ? Some(str): None;
      | _ => None;
    }
  } else {
    None;
  }
};

let validateHcl: string => hcl = str => {
  if (Str.string_match(regexp("^#[0-9a-f][0-9a-f][0-9a-f][0-9a-f][0-9a-f][0-9a-f]$"), str, 0)) {
    Some(str);
  } else {
    None;
  }
};

let validateEcl: string => ecl = str => {
  switch (str) {
    | "amb" => Some(str)
    | "blu" => Some(str)
    | "brn" => Some(str)
    | "gry" => Some(str)
    | "grn" => Some(str)
    | "hzl" => Some(str)
    | "oth" => Some(str)
    | _ => None;
  }
};

let validatePid: string => pid = str => {
  if (String.length(str) == 9 && Str.string_match(regexp("^[0-9]*$"), str, 0)) {
    Some(str);
  } else {
    None;
  }
}

let makePassport = passport => {
  let fields = passport
    |> split(regexp(" \\|\n"));

  fields |> List.fold_left((acc, x) => {
    let parsedField = x |> split(regexp(":"));
    switch (parsedField) {
      | ["byr", value, ..._] => { ...acc, byr: validateByr(value) }
      | ["iyr", value, ..._] => { ...acc, iyr: validateIyr(value) }
      | ["eyr", value, ..._] => { ...acc, eyr: validateEyr(value) }
      | ["hgt", value, ..._] => { ...acc, hgt: validateHgt(value) }
      | ["hcl", value, ..._] => { ...acc, hcl: validateHcl(value) }
      | ["ecl", value, ..._] => { ...acc, ecl: validateEcl(value) }
      | ["pid", value, ..._] => { ...acc, pid: validatePid(value) }
      | ["cid", value, ..._] => { ...acc, cid: Some(value) }
      | _ => acc
    }
  }, {
      byr: None,
      iyr: None,
      eyr: None,
      hgt: None,
      hcl: None,
      ecl: None,
      pid: None,
      cid: None,
    });
};

let isValidPassport: passport => bool = passport => {
  switch (passport) {
    | { byr: None, _ } => false
    | { iyr: None, _ } => false
    | { eyr: None, _ } => false
    | { hgt: None, _ } => false
    | { hcl: None, _ } => false
    | { ecl: None, _ } => false
    | { pid: None, _ } => false
    | { cid: Some(_val), _ } => true
    | _ => true
  }
};

let printValue: option(string) => string = data => switch (data) {
  | Some(value) => value
  | None => "N/A"
};

let sanitizedPassports = passports |> List.map(makePassport);

let nbOfValidPassports = sanitizedPassports
  |> List.fold_left((acc, curr) => {
    curr |> isValidPassport ? acc + 1 : acc;
  }, 0);

nbOfValidPassports |> string_of_int |> print_endline;
