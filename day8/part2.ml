open Printf

let input = "input.txt"

let string_to_digit_list str =
    (List.filter (fun digit -> digit <> "") (String.split_on_char ' ' str))
;;

let count_number_of_char_not_contained s1 s2 =
    (Seq.fold_left (+) 0 (Seq.map (fun c -> if(String.contains s2 c) then 0 else 1) (String.to_seq s1)))
;;

let signal_pattern_contains s1 s2 =
    (count_number_of_char_not_contained s2 s1) = 0
;;

let differentiate_0_6_9 signalPattern signalPatternsByDigits =
    if(not (Hashtbl.mem signalPatternsByDigits 1))
    then -1
    else (
        let signalPattern1 = (Hashtbl.find signalPatternsByDigits 1) in
        if (not (signal_pattern_contains signalPattern signalPattern1))
        then 6
        else if(not (Hashtbl.mem signalPatternsByDigits 4))
            then -1
            else (
                let signalPattern4 =  (Hashtbl.find signalPatternsByDigits 4) in
                if((signal_pattern_contains signalPattern signalPattern4)) then 9 else 0
            )
    )
;;

let differentiate_2_5_3 signalPattern signalPatternsByDigits =
    if(not (Hashtbl.mem signalPatternsByDigits 1))
    then -1
    else (
        let signalPattern1 = (Hashtbl.find signalPatternsByDigits 1) in
        if (signal_pattern_contains signalPattern signalPattern1)
        then 3
        else if(not (Hashtbl.mem signalPatternsByDigits 4))
            then -1
            else (
                let signalPattern4 =  (Hashtbl.find signalPatternsByDigits 4) in
                if((count_number_of_char_not_contained signalPattern signalPattern4) = 2) then 5 else 2
            )
    )
;;

let decode_signal_pattern signalPattern signalPatternsByDigits =
    let signalPatternLength = (String.length signalPattern) in
    match signalPatternLength with
        | 2 -> 1
        | 3 -> 7
        | 4 -> 4
        | 7 -> 8
        | 6 -> (differentiate_0_6_9 signalPattern signalPatternsByDigits)
        | _ -> (differentiate_2_5_3 signalPattern signalPatternsByDigits)
;;

let decode_signal_patterns codedSignalPatterns =
    let rec aux remainingUncodedSignalPatterns digitsBySignalPatterns signalPatternsByDigits signalPatternsUnableToUncode =
        match (remainingUncodedSignalPatterns, signalPatternsUnableToUncode) with
            | ([], []) -> (digitsBySignalPatterns, signalPatternsByDigits)
            | ([], _) -> (aux signalPatternsUnableToUncode digitsBySignalPatterns signalPatternsByDigits [])
            | (h::t, _) ->
                let decodedSignalPattern = (decode_signal_pattern h signalPatternsByDigits) in
                if (decodedSignalPattern = -1)
                then (aux t digitsBySignalPatterns signalPatternsByDigits (h::signalPatternsUnableToUncode))
                else (
                    (Hashtbl.add digitsBySignalPatterns h decodedSignalPattern);
                    (Hashtbl.add signalPatternsByDigits decodedSignalPattern h);
                    (aux t digitsBySignalPatterns signalPatternsByDigits signalPatternsUnableToUncode)
                )
    in

    (aux codedSignalPatterns  (Hashtbl.create 10) (Hashtbl.create 10) [])
;;

let decode_output output digitsBySignalPatterns signalPatternsByDigits =
    (List.fold_left (fun number digit -> number * 10 + digit) 0 (
        List.map (fun digit ->
            if (Hashtbl.mem digitsBySignalPatterns digit)
            then (Hashtbl.find digitsBySignalPatterns digit)
            else (
                let decodedDigit = (decode_signal_pattern digit signalPatternsByDigits) in
                (Hashtbl.add digitsBySignalPatterns digit decodedDigit);
                decodedDigit
            )
        ) output)
    )
;;

let rec input_lines outputsSum file =
   match try (input_line file) with End_of_file -> "" with
      | "" -> outputsSum
      | line ->
        let lineSplit = (String.split_on_char '|' line) in
        let signalPatterns = (string_to_digit_list (List.nth lineSplit 0)) in
        let (digitsBySignalPatterns, signalPatternsByDigits) = (decode_signal_patterns signalPatterns) in
        let output = (string_to_digit_list (List.nth lineSplit 1)) in
        let decodedOutput = (decode_output output digitsBySignalPatterns signalPatternsByDigits) in
        (input_lines (outputsSum + decodedOutput) file)
    ;;

print_int (input_lines 0 (open_in input));;