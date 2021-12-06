open Printf

let input = "input.txt"

let rec input_lines file =
   match try (input_line file) with End_of_file -> "" with
      | "" -> 0
      | line -> (input_lines file)
    ;;

print_int (input_lines (open_in input));;