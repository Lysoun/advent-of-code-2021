open Printf
open String
open List

let input = "input.txt"

let compute_new_aim instruction units aim = match instruction with
    | "down" -> aim + units
    | "up" -> aim - units
    | _ -> aim
;;

let move_submarine instruction units (x,y) aim = match instruction with
    | "forward" -> (x + units, y + units * aim)
    | _ -> (x, y)
;;

let rec input_lines file (x, y) aim =
   match try (input_line file) with End_of_file -> "" with
      | "" -> x * y
      | line ->
        let splittedLine = (String.split_on_char ' ' line) in
        let instruction = (List.nth splittedLine 0) in
        let units = (int_of_string (List.nth splittedLine 1)) in
        if (instruction = "forward")
        then (input_lines file (move_submarine instruction units (x, y) aim) aim)
        else (input_lines file (x, y) (compute_new_aim instruction units aim))
    ;;

print_int (input_lines (open_in input) (0, 0) 0);;