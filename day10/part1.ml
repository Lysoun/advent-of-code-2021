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

let illegal_character_score illegalCharacter = match illegalCharacter with
  | ')' -> 3
  | ']' -> 57
  | '>' -> 25137
  | _ -> 1197
;;

let compute_corrupted_line_score line =
    let rec aux charactersList openingCharacters = match charactersList with
        | [] -> 0
        | h::t when (is_opening_character h) -> (aux t (h::openingCharacters))
        | h::t ->
            if(is_corresponding_closing_character h (List.hd openingCharacters))
            then (aux t (List.tl openingCharacters))
            else (illegal_character_score h)
    in

    let charactersList =  (List.of_seq (String.to_seq line)) in
    (aux charactersList [])
;;

let rec input_lines file illegalCharactersScore =
   match try (input_line file) with End_of_file -> "" with
      | "" -> illegalCharactersScore
      | line -> (input_lines file (illegalCharactersScore + (compute_corrupted_line_score line)))
    ;;

print_int (input_lines (open_in input) 0);;