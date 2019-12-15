(* --- Day 4: Secure Container ---
 *
 * You arrive at the Venus fuel depot only to discover it's protected by
 * a password. The Elves had written the password on a sticky note, but
 * someone threw it out.
 *
 * However, they do remember a few key facts about the password:
 *
 *     It is a six-digit number.
 *     The value is within the range given in your puzzle input.
 *     Two adjacent digits are the same (like 22 in 122345).
 *     Going from left to right, the digits never decrease;
 *     they only ever increase or stay the same (like 111123 or 135679).
 *
 * Other than the range rule, the following are true:
 *
 *     111111 meets these criteria (double 11, never decreases).
 *     223450 does not meet these criteria (decreasing pair of digits 50).
 *     123789 does not meet these criteria (no double).
 *
 * How many different passwords within the range given in your puzzle
 * input meet these criteria? *)

open Base

let has_two_identical_adjacent_digits n =
  let str = Int.to_string n in
  let digits = List.init (String.length str) ~f:(String.get str) in
  let rec loop chars =
    match chars with
    | [] -> []
    | [_] -> []
    | x :: y :: rest -> Char.equal x y :: loop (y :: rest)
  in
  loop digits |> List.fold ~init:false ~f:(fun a b -> a || b)
;;

let are_digits_increasing n =
  let str = Int.to_string n in
  let digits = List.init (String.length str) ~f:(String.get str) in
  List.equal (List.sort digits ~compare:Char.compare) digits ~equal:Char.equal
;;

let part1 () =
  let rec count_possible_passwords acc n =
    if n = 713787
    then acc
    else
      count_possible_passwords
        ( acc
        + if are_digits_increasing n && has_two_identical_adjacent_digits n then 1 else 0
        )
        (n + 1)
  in
  count_possible_passwords 0 236491
;;

(* --- Part Two ---
 *
 *
 * An Elf just remembered one more important detail: the two adjacent
 * matching digits are not part of a larger group of matching digits.
 *
 * Given this additional criterion, but still ignoring the range rule,
 * the following are now true:
 *
 *     112233 meets these criteria because the digits never decrease and all repeated digits are exactly two digits long.
 *     123444 no longer meets the criteria (the repeated 44 is part of a larger group of 444).
 *     111122 meets the criteria (even though 1 is repeated more than twice, it still contains a double 22).
 *
 * How many different passwords within the range given in your puzzle
 * input meet all of the criteria? *)

let partition digits =
  let rec _partition acc digits =
    match digits with
    | [] -> acc
    | [_] -> acc
    | x :: y :: rest ->
      if Char.equal x y
      then
        match acc with
        | [] -> _partition acc (y :: rest)
        | a :: tail -> _partition ([y :: a] @ tail) (y :: rest)
      else _partition ([y] :: acc) (y :: rest)
  in
  match List.hd digits with None -> [[]] | Some d -> _partition [[d]] digits
;;

(* checks if the two adjacent matching digits are not part of a larger group of matching digits *)
let has_two_identical_adjacent_digits_within_group n =
  let str = Int.to_string n in
  let digits = List.init (String.length str) ~f:(String.get str) in
  List.exists (partition digits) ~f:(fun l -> List.length l = 2)
;;

let part2 () =
  let rec count_possible_passwords acc n =
    if n = 713787
    then acc
    else
      count_possible_passwords
        ( acc
        +
        if are_digits_increasing n && has_two_identical_adjacent_digits_within_group n
        then 1
        else 0 )
        (n + 1)
  in
  count_possible_passwords 0 236491
;;

let () =
  Stdio.printf "Part 1: %d\n" @@ part1 ();
  Stdio.printf "Part 2: %d\n" @@ part2 ()
;;
