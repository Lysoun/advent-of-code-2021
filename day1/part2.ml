open Printf

let input = "input.txt"

let rec input_lines file secondToLastMeasurement previousMeasurement lastSum increaseNumber =
   match try (input_line file) with End_of_file -> "" with
      | "" -> increaseNumber
      | line ->
        let measurement = (int_of_string line) in
        let newSum = secondToLastMeasurement + previousMeasurement + measurement in
        if newSum > lastSum
        then (input_lines file previousMeasurement measurement newSum (increaseNumber + 1))
        else (input_lines file previousMeasurement measurement newSum increaseNumber)
    ;;

let inputFile = (open_in input);;
let first = (int_of_string (input_line inputFile));;
let second = (int_of_string (input_line inputFile));;
let third = (int_of_string (input_line inputFile));;
print_int (input_lines inputFile second third (first + second + third) 0);;