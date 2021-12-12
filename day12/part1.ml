open Printf

let input = "input.txt"

let print_bool bool = print_string (if bool then "true" else "false");;

let is_small_cave cave =
    let firstChar = (String.get cave 0) in
    (firstChar >= 'a') && (firstChar <= 'z')
;;

let add_in_caves_if_missing cave caves =
    if (not (Hashtbl.mem caves cave)) then (Hashtbl.add caves cave [])
;;

let count_paths caves =
    let rec count_paths_aux nextCave alreadyProcessedSmallCaves = match nextCave with
        | "end" -> 1
        | _ when (Hashtbl.mem alreadyProcessedSmallCaves nextCave) -> 0
        | _ ->
            (
                (if (is_small_cave nextCave)
                then (Hashtbl.add alreadyProcessedSmallCaves nextCave 0));
                (List.fold_left
                    (fun pathsNumber neighbour ->
                        pathsNumber + (count_paths_aux neighbour (Hashtbl.copy alreadyProcessedSmallCaves)))
                    0
                    (Hashtbl.find caves nextCave)
                )
             )
    in
    (count_paths_aux "start" (Hashtbl.create 20))
;;

let print_cave (cave,neighbours) =
    (print_string cave);
    (print_string ": ");
    (List.iter (fun neighbour -> (print_string neighbour); (print_string ", ")) neighbours);
    (print_string "\n")
;;

let add_path_in_caves caves startCave endCave =
    (Hashtbl.replace caves startCave (endCave::(Hashtbl.find caves startCave)))
;;

let add_connection_between_caves caves cave1 cave2 =
    (add_path_in_caves caves cave1 cave2);
    (add_path_in_caves caves cave2 cave1);
;;

let rec input_lines file caves =
   match try (input_line file) with End_of_file -> "" with
      | "" -> (count_paths caves)
      | line ->
          let path = (String.split_on_char '-' line) in
          let pathStart = (List.nth path 0) in
          let pathEnd = (List.nth path 1) in
          (add_in_caves_if_missing pathStart caves);
          (add_in_caves_if_missing pathEnd caves);
          (add_connection_between_caves caves pathStart pathEnd);
          (input_lines file caves)
    ;;

print_int (input_lines (open_in input) (Hashtbl.create 22));;