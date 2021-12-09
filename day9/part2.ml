open Printf

let input = "input.txt"

let compute_neighbours_positions x y =
    [(x + 1, y); (x - 1, y); (x, y + 1); (x, y - 1)]
;;

let is_low_point (x, y) value heightMap =
    let neighboursPositions = (compute_neighbours_positions x y) in
    let neighboursValues = (List.map (fun pos -> (if(Hashtbl.mem heightMap pos) then Hashtbl.find heightMap pos else 10)) neighboursPositions) in
    (List.fold_left (fun isLowPoint neighbourValue -> isLowPoint && (value < neighbourValue)) true neighboursValues)
;;

let compute_low_point_risk value = value + 1;;

let print_position (x, y) =
    (print_string "x: ");
    (print_int x);
    (print_string ";y: ");
    (print_int y);
    (print_string "\n")
;;

let compute_bassin_size (lowPointX, lowPointY) heightMap =
    let rec aux bassinSize toProcessPositions alreadyProcessedPositions  = match toProcessPositions with
        | [] -> bassinSize
        | h::t when (Hashtbl.mem alreadyProcessedPositions h ) ->  (aux bassinSize t alreadyProcessedPositions)
        | h::t when ((not (Hashtbl.mem heightMap h)) || ((Hashtbl.find heightMap h ) = 9)) -> (aux bassinSize t alreadyProcessedPositions)
        | (x, y)::t ->
            (Hashtbl.add alreadyProcessedPositions (x,y) 0);
            let neighboursUnprocessed = (List.filter (fun pos -> (not (Hashtbl.mem alreadyProcessedPositions pos))) (compute_neighbours_positions x y)) in
            (aux (bassinSize + 1) (neighboursUnprocessed@t) alreadyProcessedPositions)
    in
    let alreadyProcessedPositions = Hashtbl.create 4 in
    (Hashtbl.add alreadyProcessedPositions (lowPointX, lowPointY) 0);
    (aux 1 (compute_neighbours_positions lowPointX lowPointY) alreadyProcessedPositions)
;;
let compute_largest_bassins_product heightMap =
    let bassins =
        (Seq.map
            (fun (pos, value) ->
                if(is_low_point pos value heightMap)
                then (compute_bassin_size pos heightMap)
                else 0
             )
             (Hashtbl.to_seq heightMap)
        )
    in

    let sortedBassinsSizes = (List.fast_sort (fun a b -> if a = b then 0 else if a < b  then 1 else -1) (List.of_seq bassins)) in
    (List.fold_left (fun a b -> a * b) 1 (List.map (fun index -> (List.nth sortedBassinsSizes index)) [0; 1; 2] ))
;;

let char_to_int char = (int_of_char char) - 48;;

let rec input_lines file heightMap lineNumber =
   match try (input_line file) with End_of_file -> "" with
      | "" -> (compute_largest_bassins_product heightMap)
      | line ->
        let linePointsValues = (List.map (char_to_int) (List.of_seq (String.to_seq line))) in
        (List.iteri (fun index value -> (Hashtbl.add heightMap (lineNumber, index) value)) linePointsValues);
        (input_lines file heightMap (lineNumber + 1))
    ;;

print_int (input_lines (open_in input) (Hashtbl.create 1000) 0);;