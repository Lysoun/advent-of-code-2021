open Printf

let input = "input.txt"

let compute_neighbours_positions (x, y) =
        [(x + 1, y); (x - 1, y); (x, y + 1); (x, y - 1); (x + 1, y + 1); (x - 1, y + 1); (x - 1, y - 1); (x + 1, y - 1)]
;;

let print_position (x, y) =
    (print_string "x: ");
    (print_int x);
    (print_string ";y: ");
    (print_int y);
    (print_string " ")
;;

let print_octopuses dumboOctopuses =
    (Seq.iter (fun (pos, level) -> ((print_position pos); (print_int level); (print_string "\n"))) (Hashtbl.to_seq dumboOctopuses))
;;

let retrieve_octopuses_about_to_flash hashtbl =
    let octopusesCopy = (Hashtbl.copy hashtbl) in
     (Hashtbl.filter_map_inplace
                    (fun pos octopusLevel -> if octopusLevel > 9 then (Option.some octopusLevel) else None)
                    octopusesCopy
                );
    (List.of_seq (Hashtbl.to_seq_keys octopusesCopy))
;;

let end_step dumboOctopuses alreadyFlashedOctopuses =
    (Seq.iter (fun pos -> (Hashtbl.replace dumboOctopuses pos 0)) (Hashtbl.to_seq_keys alreadyFlashedOctopuses));
    let flashesNumber = (Seq.fold_left (fun sum pos -> sum + 1) 0 (Hashtbl.to_seq_keys alreadyFlashedOctopuses)) in
    (dumboOctopuses, flashesNumber)
;;


let execute_step dumboOctopuses =
    let rec aux octopusesToProcess alreadyFlashedOctopuses = match octopusesToProcess with
        | [] -> (end_step dumboOctopuses alreadyFlashedOctopuses)
        | pos::tl when (((Hashtbl.find dumboOctopuses pos) > 9) && (not (Hashtbl.mem alreadyFlashedOctopuses pos))) ->
            (
                (Hashtbl.add alreadyFlashedOctopuses pos 0);
                let neighbours = (List.filter (fun pos -> (Hashtbl.mem dumboOctopuses pos)) (compute_neighbours_positions pos)) in
                (List.iter (fun pos -> (Hashtbl.replace dumboOctopuses pos (1 + (Hashtbl.find dumboOctopuses pos)))) neighbours);
                (aux (neighbours@octopusesToProcess) alreadyFlashedOctopuses)
            )
        | pos::tl -> (aux tl alreadyFlashedOctopuses)
    in

    let octopusesAboutToFlash = (retrieve_octopuses_about_to_flash dumboOctopuses) in
    (aux octopusesAboutToFlash (Hashtbl.create 100))
;;

let increase_all_octopuses dumboOctopuses =
    (Seq.iter
        (fun (pos, octopusLevel) -> (Hashtbl.replace dumboOctopuses pos (octopusLevel + 1)))
        (Hashtbl.to_seq dumboOctopuses)
    );
    dumboOctopuses
;;


let count_flashes_after_steps numberOfSteps dumboOctopuses =
    let rec count_flashes_during_steps numberOfSteps flashesNumber dumboOctopuses = match numberOfSteps with
        | 0 -> flashesNumber
        | _ ->
            let (dumboOctopusesAfterStep, stepFlashesNumber) = (execute_step (increase_all_octopuses dumboOctopuses)) in
            (count_flashes_during_steps (numberOfSteps - 1) (flashesNumber + stepFlashesNumber) dumboOctopusesAfterStep)
    in

    (count_flashes_during_steps numberOfSteps 0 dumboOctopuses)
;;

let char_to_int char = (int_of_char char) - 48;;

let rec input_lines file dumboOctopuses lineNumber =
   match try (input_line file) with End_of_file -> "" with
      | "" -> (count_flashes_after_steps 100 dumboOctopuses)
      | line ->
        let linePointsValues = (List.map (char_to_int) (List.of_seq (String.to_seq line))) in
        (List.iteri (fun index value -> (Hashtbl.add dumboOctopuses (lineNumber, index) value)) linePointsValues);
        (input_lines file dumboOctopuses (lineNumber + 1))
    ;;

print_int (input_lines (open_in input) (Hashtbl.create 100) 0);;