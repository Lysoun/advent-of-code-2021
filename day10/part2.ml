open Printf

let input = "input.txt"

let is_opening_character character =
    (List.fold_left (fun result openingCharacter -> result || (character =openingCharacter)) false ['('; '['; '<'; '{'])
;;

let corresponding_closing_character openingCharacter = match openingCharacter with
    | '(' -> ')'
    | '[' -> ']'
    | '<' -> '>'
    | _ -> '}'
;;

let is_corresponding_closing_character character openingCharacter = character = (corresponding_closing_character openingCharacter);;

let missing_character_score missingCharacter = match missingCharacter with
  | ')' -> 1
  | ']' -> 2
  | '>' -> 4
  | _ -> 3
;;

let compute_missing_characters_score unclosedOpeningCharacters =
    List.fold_left
        (fun score missingClosingCharacter -> (5 * score) + (missing_character_score missingClosingCharacter))
        0
        (List.map (corresponding_closing_character) unclosedOpeningCharacters)
;;

let compute_line_score line =
    let rec aux charactersList openingCharacters = match charactersList with
        | [] -> (compute_missing_characters_score openingCharacters)
        | h::t when (is_opening_character h) -> (aux t (h::openingCharacters))
        | h::t ->
            if(is_corresponding_closing_character h (List.hd openingCharacters))
            then (aux t (List.tl openingCharacters))
            else 0
    in

    let charactersList =  (List.of_seq (String.to_seq line)) in
    (aux charactersList [])
;;

let rec input_lines file scores =
   match try (input_line file) with End_of_file -> "" with
      | "" ->
        let sortedScores = (List.sort (fun a b -> if a = b then 0 else if a > b then 1 else -1) scores) in
        (List.nth sortedScores ((List.length sortedScores) /2))
      | line ->
        let lineScore = (compute_line_score line) in
        if(lineScore > 0) then
            (input_lines file (lineScore::scores))
        else
            (input_lines file scores)
    ;;

print_int (input_lines (open_in input) []);;