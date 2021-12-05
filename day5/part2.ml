open Printf

let input = "input.txt";;

let print_coordinates (x, y) =
    (print_string "(");
    (print_int x);
    (print_string ";");
    (print_int y);
    (print_string ")");
;;

let count_overlapping_points vents =
    (Seq.fold_left (fun sum ventNumber -> sum + 1) 0 (Seq.filter (fun ventNumber -> ventNumber > 1) (Hashtbl.to_seq_values vents)))
;;

let rec range a b =
    if(b < a)
    then (List.rev (range b a))
    else (List.init (b - a + 1) (fun index -> a + index))
;;

let diagonal_range (x1, y1) (x2, y2) =
    let rangeX = (range x1 x2) in
    let rangeY = (range y1 y2) in
    (List.combine rangeX rangeY)
;;

let coordinates_to_range (x1, y1) (x2, y2) =
    let xDifference = x2 - x1 in
    let yDifference = y2 - y1 in

    if(xDifference = 0 && yDifference <> 0)
    then (List.map (fun y -> (x1, y)) (range y1 y2))
    else if (yDifference = 0 && xDifference <> 0)
        then (List.map (fun x -> (x, y1)) (range x1 x2))
        else (diagonal_range (x1, y1) (x2, y2))
;;

let string_to_coordinates stringCoordinates =
    let coordinatesList = (List.map (int_of_string) (String.split_on_char ',' stringCoordinates)) in
    ((List.nth coordinatesList 0), (List.nth coordinatesList 1))
;;

let line_to_coordinates line =
    let splittedLine = (String.split_on_char ' ' line) in
    ((string_to_coordinates (List.nth splittedLine 0)), (string_to_coordinates (List.nth splittedLine 2)))
;;

let add_vent ventPosition vents =
    if (Hashtbl.mem vents ventPosition)
    then Hashtbl.replace vents ventPosition ((Hashtbl.find vents ventPosition) + 1)
    else Hashtbl.add vents ventPosition 1
;;


let rec input_lines file vents =
   match try (input_line file) with End_of_file -> "" with
      | "" -> (count_overlapping_points vents)
      | line -> let (p1, p2) = (line_to_coordinates line) in
        let lineVent = (coordinates_to_range p1 p2) in
        (List.iter (fun ventPosition -> (add_vent ventPosition vents)) lineVent);
        (input_lines file vents)
    ;;

print_int (input_lines (open_in input) (Hashtbl.create 500));;