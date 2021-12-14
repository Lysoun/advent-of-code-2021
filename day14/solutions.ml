open Printf

let input = "input.txt";;
let file = (open_in input);;

let decrease_count hashtbl key countToSubtract =
    let count = (Hashtbl.find hashtbl key) in

    if (count = countToSubtract)
    then (Hashtbl.remove hashtbl key)
    else (Hashtbl.replace hashtbl key (count - countToSubtract))
;;

let increase_count hashtbl key toAdd =
    if(Hashtbl.mem hashtbl key)
    then (Hashtbl.replace hashtbl key ((Hashtbl.find hashtbl key) + toAdd))
    else (Hashtbl.add hashtbl key toAdd)
;;

let polymer_to_letters_pairs polymer =
    let polymerLength = (String.length polymer) in
    let rec polymer_list_to_letters_pairs index lettersPairs =
        if(index >= (polymerLength - 1))
        then lettersPairs
        else
            let pair = (String.sub polymer index 2) in
            (increase_count lettersPairs pair 1);
            (polymer_list_to_letters_pairs (index + 1) lettersPairs)
    in
    (polymer_list_to_letters_pairs 0 (Hashtbl.create 20))
;;

let polymerPairs = (polymer_to_letters_pairs (input_line file));;

(* Skip empty line *)
(input_line file)

let rec read_pair_insertion_rules pairInsertionRules =
   match try (input_line file) with End_of_file -> "" with
      | "" -> pairInsertionRules
      | line ->
        let split = (String.split_on_char ' ' line) in
        let newCharacter = (List.nth split 2) in
        let pairTransformed = (List.nth split 0) in
        let firstNewPair = (String.concat "" [(String.sub pairTransformed 0 1); newCharacter]) in
        let secondNewPair = (String.concat "" [newCharacter; (String.sub pairTransformed 1 1)]) in
        (Hashtbl.add pairInsertionRules pairTransformed ([firstNewPair; secondNewPair]));
        (read_pair_insertion_rules pairInsertionRules)
    ;;

let pairInsertionRules = (read_pair_insertion_rules (Hashtbl.create 100));;

let apply_step polymerPairs pairInsertionRules =
    let polymerPairsAfterStep = (Hashtbl.copy polymerPairs) in
    (Seq.iter
        (fun (pair, count) ->
            let createdPairs = (Hashtbl.find pairInsertionRules pair) in
            (List.iter (fun pair -> (increase_count polymerPairsAfterStep pair count)) createdPairs);
            (decrease_count polymerPairsAfterStep pair count)
        )
        (Hashtbl.to_seq polymerPairs)
    );
    polymerPairsAfterStep
;;

let rec apply_steps stepsNumber polymerTemplate pairInsertionRules =
    if(stepsNumber = 0)
    then polymerTemplate
    else (apply_steps (stepsNumber - 1) (apply_step polymerTemplate pairInsertionRules) pairInsertionRules)
;;

let polymer_pairs_to_letters_count polymer =
    let lettersCount = (Hashtbl.create 50) in
    (Seq.iter
        (fun (pair, count) -> (increase_count lettersCount (String.get pair 1) count))
        (Hashtbl.to_seq polymer)
    );
    lettersCount
;;

let compute_min_and_max lettersCount =
    (Seq.fold_left
        (fun (min, max) count ->
            if (count < min)
            then (count, max)
            else (
                if (count > max)
                then (min, count)
                else (min, max)
            )
        )
        (1000000000000, 0)
        (Hashtbl.to_seq_values lettersCount)
    )
;;

let stepsNumber = 40;;

let polymerAfterSteps = (apply_steps stepsNumber polymerPairs pairInsertionRules);;
let lettersCount = (polymer_pairs_to_letters_count polymerAfterSteps);;

let (min, max) = (compute_min_and_max lettersCount);;
(print_int (max - min));;