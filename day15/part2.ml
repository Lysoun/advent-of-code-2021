open Printf

let input = "input.txt";;

let get_neighbours (x, y) =
    [(x + 1, y); (x, y + 1); (x - 1, y); (x, y - 1)]
;;

let char_to_int char = (int_of_char char) - 48;;
let inputSize = 100;;
let mapSize = 499;;

let endPoint = (mapSize, mapSize);;
let startPoint = (0, 0);;

let print_position (x, y) =
    (print_string "x: ");
    (print_int x);
    (print_string ";y: ");
    (print_int y);
    (print_string "\n")
;;

let pos_to_risk_map_pos (x, y) =
    (x mod inputSize, y mod inputSize)
;;

let get_risk riskMap (x, y) =
    let riskMapPos = (pos_to_risk_map_pos (x, y)) in
    let xOffset = (x / inputSize) in
    let yOffset = (y / inputSize) in
    let riskWithOffset = ((Hashtbl.find riskMap riskMapPos) + xOffset + yOffset) in
    let risk =
       if(riskWithOffset <= 9)
       then riskWithOffset
       else (riskWithOffset mod 9)
    in
    risk
;;

let print_map riskMap =
    for y = 0 to mapSize do
        for x = 0 to mapSize do
            (print_int (get_risk riskMap (x, y)))
        done;
        (print_string "\n")
    done
;;

let find_or_default tbl default key =
    if (Hashtbl.mem tbl key) then (Hashtbl.find tbl key) else default
;;

let compare_positions paths pos1 pos2 =
    (compare (find_or_default paths (-1) pos1) (find_or_default paths (-1) pos2))
;;

let find_less_risky_way riskMap =
    let paths = (Hashtbl.create 500000) in
    (Hashtbl.add paths (0, 0) 0);

    let rec compute_risks positionsToVisit = match positionsToVisit with
        | [] -> ()
        | nextPositionToCheck::t ->
            let currentPointRisk = (Hashtbl.find paths nextPositionToCheck) in
            let neighbours = (List.filter
                (fun (x,y) -> (x >= 0 && y >= 0 && x <= mapSize && y <= mapSize))
                (get_neighbours nextPositionToCheck)
            ) in
            let neighboursVisited = (List.filter
                (fun pos -> (
                    let positionCurrentRisk =
                        if (Hashtbl.mem paths pos)
                        then (Hashtbl.find paths pos)
                        else -1
                    in
                    let potentialNewRisk = currentPointRisk + (get_risk riskMap pos) in
                    if (positionCurrentRisk = -1) || (potentialNewRisk < positionCurrentRisk)
                    then (Hashtbl.replace paths pos potentialNewRisk; true)
                    else false
                ))
                neighbours
            ) in

            if nextPositionToCheck = endPoint
            then ()
            else (compute_risks ((List.merge (compare_positions paths) (List.fast_sort (compare_positions paths) neighboursVisited) t)))
    in

    (compute_risks [startPoint]);
    (Hashtbl.find paths endPoint)
;;

let rec input_lines file riskMap lineNumber =
   match try (input_line file) with End_of_file -> "" with
      | "" -> (find_less_risky_way riskMap)
      | line ->
        (String.iteri (fun index riskLevel -> (Hashtbl.add riskMap (index, lineNumber) (char_to_int riskLevel))) line);
        (input_lines file riskMap (lineNumber + 1))
;;

print_int (input_lines (open_in input) (Hashtbl.create 10000) 0);;