open Printf

let input = "input.txt";;

let get_neighbours (x, y) =
    [(x + 1, y); (x, y + 1); (x - 1, y); (x, y - 1)]
;;

let char_to_int char = (int_of_char char) - 48;;

let endPoint = (99, 99);;
let startPoint = (0, 0);;

let print_position (x, y) =
    (print_string "x: ");
    (print_int x);
    (print_string ";y: ");
    (print_int y);
    (print_string "\n")
;;

let find_less_risky_way riskMap =
    let paths = (Hashtbl.create 100000) in
    let positionsToVisit = (Hashtbl.create 100000) in
    (Seq.iter
        (fun pos ->
            (
                (Hashtbl.add paths pos (-1));
                (Hashtbl.add positionsToVisit pos (-1));
            )
        )
        (Hashtbl.to_seq_keys riskMap)
    );
    (Hashtbl.replace paths (0, 0) 0);

    let find_next_pos_to_check positionsToVisit =
        let (pos, risk) = (Seq.fold_left
            (fun (minPos, minRisk) (pos, risk) ->
                if (risk > -1) && (risk < minRisk)
                then (pos, risk)
                else (minPos, minRisk)
            )
            ((99, 99), 10000000)
            ((Seq.filter (fun (pos, risk) -> (Hashtbl.mem positionsToVisit pos)) (Hashtbl.to_seq paths)))
         ) in
         pos
    in

    let rec compute_risks positionsToVisit =
        let nextPositionToCheck = (find_next_pos_to_check positionsToVisit) in
        let currentPointRisk = (Hashtbl.find paths nextPositionToCheck) in
        let neighbours = (List.filter
            (fun (x,y) -> (Hashtbl.mem positionsToVisit (x, y)))
            (get_neighbours nextPositionToCheck)
        ) in
        (List.iter
            (fun pos ->
                let positionCurrentRisk = (Hashtbl.find paths pos) in
                let potentialNewRisk = currentPointRisk + (Hashtbl.find riskMap pos) in
                if (positionCurrentRisk = -1) || (potentialNewRisk < positionCurrentRisk)
                then ((Hashtbl.replace paths pos potentialNewRisk); (Hashtbl.replace positionsToVisit pos potentialNewRisk);)
            )
            neighbours
        );
        (Hashtbl.remove positionsToVisit nextPositionToCheck);

        if nextPositionToCheck = endPoint
        then ()
        else (compute_risks positionsToVisit)
    in

    (compute_risks positionsToVisit);
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