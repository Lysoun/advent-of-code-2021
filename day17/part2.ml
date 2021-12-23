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

let ((minX, maxX), (minY, maxY)) = (read_target_area ());;

let print_position (x, y) =
    (print_string "x: ");
    (print_int x);
    (print_string ";y: ");
    (print_int y);
    (print_string "\n")
;;


let will_reach_target_area (x, y) =
    let rec aux (currentXVelocity, currentYVelocity) (currentXPosition, currentYPosition) =
            (
                if (currentXPosition >= minX && currentXPosition <= maxX && currentYPosition >= minY && currentYPosition <= maxY)
                then true
                else
                    if ((currentXVelocity = 0 && (currentXPosition > maxX || currentXPosition < minX)) || ((currentYVelocity < 0) && (currentYPosition < minY)))
                    then false
                    else
                        let newXVelocity =
                            if currentXVelocity > 0
                            then (currentXVelocity - 1)
                            else if currentXVelocity < 0
                                then (currentXVelocity + 1)
                                else 0
                        in
                        let newYVelocity = currentYVelocity - 1 in
                        (aux (newXVelocity, newYVelocity) ((currentXPosition + currentXVelocity), (currentYPosition + currentYVelocity)))
            ) in
        (aux (x, y) (0,0))
;;

let count_valid_velocities () =
    let xInit = 0 in
    let yInit = -106 in
    let rec aux x y count =
        if x > 2000
        then count
        else
            if y >= 107
            then (aux (x + 1) yInit count)
            else
                let newCount =
                    if (will_reach_target_area (x, y))
                    then (count + 1)
                    else count
                in
                (aux x (y + 1) newCount)
    in
    (aux xInit yInit 0)
;;

(print_int (count_valid_velocities ()));;