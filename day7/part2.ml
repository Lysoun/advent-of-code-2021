open Printf

let input = "input.txt"
let crabsPositions = (List.map (int_of_string) (String.split_on_char ',' (input_line (open_in input))));;

let addCrabPositionInNumberOfCrabsByPosition position numberOfCrabsByPosition =
    if (Hashtbl.mem numberOfCrabsByPosition position)
    then (Hashtbl.replace numberOfCrabsByPosition position ((Hashtbl.find numberOfCrabsByPosition position) + 1))
    else (Hashtbl.add numberOfCrabsByPosition position 1)
;;

let rec fillNumberOfCrabsByPosition minPosition maxPosition positions numberOfCrabsByPosition =
    match positions with
    | [] -> (minPosition, maxPosition, numberOfCrabsByPosition)
    | h::t -> (
        (addCrabPositionInNumberOfCrabsByPosition h numberOfCrabsByPosition);
        if(h < minPosition)
        then (fillNumberOfCrabsByPosition h maxPosition t numberOfCrabsByPosition)
        else if(h > maxPosition)
            then (fillNumberOfCrabsByPosition minPosition h t numberOfCrabsByPosition)
            else (fillNumberOfCrabsByPosition minPosition maxPosition t numberOfCrabsByPosition)
    )
;;

let (min, max, numberOfCrabsByPosition) = (fillNumberOfCrabsByPosition 10000 0 crabsPositions (Hashtbl.create 10));;
let numberOfCrabsByPositionList = (List.of_seq (Hashtbl.to_seq numberOfCrabsByPosition));;

let range a b = List.init (b - a + 1) (fun index -> a + index);;

let abs a = if(a < 0) then -a else a;;

let compute_distances_with_position_sum position numberOfCrabsByPositionList =
    let rec aux sum numberOfCrabsByPositionList =
        match numberOfCrabsByPositionList with
        | [] -> sum
        | (crabsPosition, crabsNumber)::t ->
            let diff = (abs (crabsPosition - position)) in
            (aux (sum + (diff * (diff + 1) / 2) * crabsNumber) t)
    in
    (aux 0 numberOfCrabsByPositionList)
;;

let compute_list_min list =
    let rec aux min list =
        match list with
        | [] -> min
        | h::t -> if(h < min) then (aux h t) else (aux min t)
    in

    (aux (List.hd list) (List.tl list))
;;

let distancesToRange = (List.map (fun position -> (compute_distances_with_position_sum position numberOfCrabsByPositionList)) (range min max));;

print_int (compute_list_min distancesToRange);;