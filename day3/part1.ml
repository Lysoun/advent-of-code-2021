open Printf

let input = "input.txt";;
let lineSize = 12;;

let binary_to_decimal binary =
   let rec aux decimal factor index =
        if (index < 0)
        then decimal
        else (aux (decimal + factor * (Array.get binary index)) (factor * 2) (index - 1))
    in
    (aux 0 1 (lineSize - 1))
;;

let reverse number = 1 - number;;

let sum_to_majority fileLinesNumber sum = if (sum >= (fileLinesNumber / 2)) then 1 else 0;;

let columns_sums_to_power_consumption columnsSums fileLinesNumber =
    let gammaRateBinary = (Array.map (sum_to_majority fileLinesNumber) columnsSums) in
    let epsilonRateBinary = (Array.map (reverse) gammaRateBinary) in
    (binary_to_decimal gammaRateBinary) * (binary_to_decimal epsilonRateBinary)
;;

let rec add_line_to_sum line columnsSums index =
    if(index = lineSize)
    then columnsSums
    else (
        (Array.set columnsSums index ((int_of_char (String.get line index) - 48) + (Array.get columnsSums index)));
        (add_line_to_sum line columnsSums (index + 1))
    )
;;

let rec input_lines file columnsSums fileLinesNumber =
   match try (input_line file) with End_of_file -> "" with
      | "" -> (columns_sums_to_power_consumption columnsSums fileLinesNumber)
      | line -> (input_lines file (add_line_to_sum line columnsSums 0) (fileLinesNumber + 1))
    ;;

print_int (input_lines (open_in input) (Array.make lineSize 0) 0);;