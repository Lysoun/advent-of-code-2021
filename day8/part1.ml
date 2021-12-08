open Printf

let input = "input.txt"

let is_easy_digit digit =
    let digitLength = (String.length digit) in
    digitLength = 2 || digitLength = 3 || digitLength = 4 || digitLength = 7
;;

let count_output_easy_digits output =
    (List.fold_left (fun sum digit -> if (is_easy_digit digit) then (sum + 1) else sum) 0 output)
;;

let rec input_lines easyDigitsNumber file =
   match try (input_line file) with End_of_file -> "" with
      | "" -> easyDigitsNumber
      | line ->
        let output = (List.tl (String.split_on_char ' ' (List.nth (String.split_on_char '|' line) 1))) in
        (input_lines (easyDigitsNumber + (count_output_easy_digits output)) file)
    ;;

print_int (input_lines 0 (open_in input));;