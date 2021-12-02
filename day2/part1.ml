open Printf
open String
open List

let input = "input.txt"

let move instruction units (x,y) = match instruction with
    | "forward" -> (x + units, y)
    | "up" -> (x, y - units)
    | _ -> (x, y + units)
;;

let rec input_lines file (x, y) =
   match try (input_line file) with End_of_file -> "" with
      | "" -> x * y
      | line ->
        let splittedLine = (String.split_on_char ' ' line) in
        (input_lines file (move (List.nth splittedLine 0) (int_of_string (List.nth splittedLine 1)) (x, y)))
    ;;

print_int (input_lines (open_in input) (0, 0));;