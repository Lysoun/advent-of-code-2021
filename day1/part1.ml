open Printf

let input = "input.txt"

let rec input_lines file previousMeasurement increaseNumber =
   match try (input_line file) with End_of_file -> "" with
      | "" -> increaseNumber
      | line ->
        let measurement = (int_of_string line) in
        if measurement > previousMeasurement
        then (input_lines file measurement (increaseNumber + 1))
        else (input_lines file measurement increaseNumber)
    ;;

let inputFile = (open_in input);;
print_int (input_lines inputFile (int_of_string (input_line inputFile)) 0);;