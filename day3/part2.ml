open Printf

let input = "input.txt";;
let lineSize = 12;;

let char_to_int char = (int_of_char char) - 48;;

let binary_to_decimal binary =
   let rec aux decimal factor index =
        if (index < 0)
        then decimal
        else (aux (decimal + factor * (char_to_int (String.get binary index))) (factor * 2) (index - 1))
    in
    (aux 0 1 (lineSize - 1))
;;

let reverse number = 1 - number;;

let sum_to_majority fileLinesNumber sum = if (2 * sum >= fileLinesNumber) then 1 else 0;;
let sum_to_minority fileLinesNumber sum = 1 - (sum_to_majority fileLinesNumber sum);;

let lines_list_to_column_sum lines index =
    let rec aux sum lines = match lines with
        | [] -> sum
        | h::t -> aux ((char_to_int (String.get h index)) + sum) t
    in
    (aux 0 lines)
;;

let find_rating inputLines sumTransformer =
    let compute_binary lines index =
        (sumTransformer (List.length lines) (lines_list_to_column_sum lines index))
    in

    let rec filter_input remaining_lines index expectedValueForIndex = match remaining_lines with
        | [] -> []
        | h::t  when (char_to_int (String.get h index)) = expectedValueForIndex -> h::(filter_input t index expectedValueForIndex)
        | h::t -> (filter_input t index expectedValueForIndex)
    in

    let rec filter_until_one_remaining remaining_lines index =
        let expectedValueForIndex = (compute_binary remaining_lines index) in
        match (filter_input remaining_lines index expectedValueForIndex) with
            | h::[] -> h
            | l -> (filter_until_one_remaining l (index + 1))
    in

    (filter_until_one_remaining inputLines 0)
;;

let input_to_life_support_rating fileLinesNumber inputLines =
    let oxygenGeneratorRating = (binary_to_decimal (find_rating inputLines (sum_to_majority))) in
    let co2ScrubberRating = (binary_to_decimal (find_rating inputLines (sum_to_minority))) in
    oxygenGeneratorRating * co2ScrubberRating
;;

let rec input_lines file fileLinesNumber linesList =
   match try (input_line file) with End_of_file -> "" with
      | "" -> (input_to_life_support_rating fileLinesNumber linesList)
      | line -> (input_lines file (fileLinesNumber + 1) (line::linesList))
    ;;

print_int (input_lines (open_in input) 0 []);;