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


let compute_low_points_number heightMap =
    (Seq.fold_left
        (+)
        0
        (Seq.map
            (fun (pos, value) ->
                if(is_low_point pos value heightMap)
                then (compute_low_point_risk value)
                else 0
             )
             (Hashtbl.to_seq heightMap)
        )
    )
;;

let char_to_int char = (int_of_char char) - 48;;

let rec input_lines file heightMap lineNumber =
   match try (input_line file) with End_of_file -> "" with
      | "" -> (compute_low_points_number heightMap)
      | line ->
        let linePointsValues = (List.map (char_to_int) (List.of_seq (String.to_seq line))) in
        (List.iteri (fun index value -> (Hashtbl.add heightMap (lineNumber, index) value)) linePointsValues);
        (input_lines file heightMap (lineNumber + 1))
    ;;

print_int (input_lines (open_in input) (Hashtbl.create 1000) 0);;