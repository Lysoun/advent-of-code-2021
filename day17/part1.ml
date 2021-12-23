open Printf

let input = "input.txt";;

(** Read target area **)
let range_to_coordinates range =
    let rangeSplit = (String.split_on_char '.' range) in
    ((int_of_string (List.nth rangeSplit 0)), (int_of_string (List.nth rangeSplit 2)))
;;

let read_target_area () =
    let line = (input_line (open_in input)) in
    let lineSplitOnEquals = (String.split_on_char '=' line) in
    let xRange = (List.nth (String.split_on_char ',' (List.nth lineSplitOnEquals 1)) 0) in
    let (minY, maxY) = (range_to_coordinates (List.nth lineSplitOnEquals 2))in
    let (minX, maxX) = (range_to_coordinates xRange) in
    ((minX, maxX), (minY, maxY))
;;

let (targetX, targetY) = (read_target_area ());;

(** Find y velocity **)

let will_reach_target_area_with_y_velocity y =
    let (minY, maxY) = targetY in
    let rec aux currentYVelocity currentYPosition maxReached =
    (
        (print_int currentYPosition);
        (print_string "\n");
        if (currentYPosition >= minY && currentYPosition <= maxY)
        then (true, maxReached)
        else
            if (currentYPosition < minY)
            then (false, 0)
            else
                if(currentYPosition >= maxReached)
                then (aux (currentYVelocity - 1) (currentYPosition + currentYVelocity) currentYPosition)
                else (aux (currentYVelocity - 1) (currentYPosition + currentYVelocity) maxReached)
    ) in
    (aux y 0 0)
;;

let find_maximal_initial_y_velocity_to_reach_target_area () =
    let rec aux y maxReached =
        let (willReach, newMaxReached) = (will_reach_target_area_with_y_velocity y) in
        if willReach
        then (aux (y + 1) newMaxReached)
        else maxReached
    in
    (aux 100 0)
;;

(print_int (find_maximal_initial_y_velocity_to_reach_target_area ()));;