open Printf

let input = "input.txt"
let file = (open_in input);;

let print_position (x, y) =
    (print_string "x: ");
    (print_int x);
    (print_string ";y: ");
    (print_int y);
    (print_string "\n")
;;

let rec read_dots file numberOfDotsByPosition =
   match try (input_line file) with End_of_file -> "" with
      | "" -> numberOfDotsByPosition
      | line ->
        let coordinates = (List.map (int_of_string) (String.split_on_char ',' line)) in
        (Hashtbl.add numberOfDotsByPosition ((List.nth coordinates 0), (List.nth coordinates 1)) 1);
        (read_dots file numberOfDotsByPosition)
    ;;

let numberOfDotsByPosition = read_dots file (Hashtbl.create 847);;

let fold pointsFilter positionTransformer numberOfDotsByPosition =
    let pointsToFold = (List.filter (fun ((x, y), numberOfDots) -> (pointsFilter (x, y))) (List.of_seq (Hashtbl.to_seq numberOfDotsByPosition))) in
        (List.iter
            (fun ((x, y), numberOfDots) -> (
                let newPosition = (positionTransformer (x,y)) in

                if(Hashtbl.mem numberOfDotsByPosition newPosition)
                then (Hashtbl.replace numberOfDotsByPosition newPosition ((Hashtbl.find numberOfDotsByPosition newPosition) + numberOfDots))
                else (Hashtbl.add numberOfDotsByPosition newPosition 1);

                (Hashtbl.remove numberOfDotsByPosition (x, y))
            ))
            pointsToFold
        )
;;

let fold_horizontally column numberOfDotsByPosition =
    (fold (fun (x, y) -> x >= column) (fun (x, y) -> (2 * column - x, y)) numberOfDotsByPosition)
;;

let fold_vertically row numberOfDotsByPosition =
    (fold (fun (x, y) -> y >= row) (fun (x, y) -> (x, 2 * row - y)) numberOfDotsByPosition)
;;

let rec execute_all_folding_instructions file numberOfDotsByPosition =
    match try (input_line file) with End_of_file -> "" with
     | "" -> numberOfDotsByPosition
     | line ->
        let foldingInstruction = (String.split_on_char '=' (List.nth (String.split_on_char ' ' line) 2)) in
        let lineToFold = (int_of_string (List.nth foldingInstruction 1)) in
        if (List.nth foldingInstruction 0) = "x"
        then (fold_horizontally lineToFold numberOfDotsByPosition)
        else (fold_vertically lineToFold numberOfDotsByPosition);
       (execute_all_folding_instructions file numberOfDotsByPosition)
;;

let print_folded_paper numberOfDotsByPosition =
    for y = 0 to 5 do
        for x = 0 to 40 do
            if(Hashtbl.mem numberOfDotsByPosition (x, y)) then print_string ("█") else (print_string " ")
        done;
        (print_string "\n")
    done
;;

(print_folded_paper (execute_all_folding_instructions file numberOfDotsByPosition));;