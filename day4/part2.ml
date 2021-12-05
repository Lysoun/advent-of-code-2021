open Printf

let input = "input.txt";;

type cell = {number:int; isMarked:bool};;

let init_cell number = {number=number; isMarked=false};;

let file = (open_in input);;

let numbersDrawn = (List.map (int_of_string) (String.split_on_char ',' (input_line file)));;

let print_int_list intList = (List.iter (print_int) intList);;
let print_int_array intArray = (Array.iter (print_int) intArray);;
let print_cell cell = (print_int cell.number);(print_string ";");(Format.print_bool cell.isMarked);(print_string "  ");;
let print_cell_array cellArray = (Array.iter (print_cell) cellArray);;
let print_board board = (print_cell_array (Array.get board 0));
                        (print_string "\n");
                            (print_cell_array (Array.get board 1));
                        (print_string "\n");
                            (print_cell_array (Array.get board 2));
                        (print_string "\n");
                            (print_cell_array (Array.get board 3));
                        (print_string "\n");
                            (print_cell_array (Array.get board 4));
                        (print_string "\n");;

let range = [0; 1; 2; 3; 4];;

let is_winning_board_horizontally board =
    (Array.fold_left (fun res line -> res || (Array.for_all (fun cell -> cell.isMarked) line)) false board)
;;

let is_winning_board_vertically board =
    (List.fold_left
        (fun res x ->
            res ||
            (List.fold_left (&&) true (List.map (fun y -> board.(y).(x).isMarked) range))
        )
        false
        range
    )
;;

let is_winning_board board =
    let isWinningBoardHorizontally = is_winning_board_horizontally board in
    let isWinningBoardVertically = is_winning_board_vertically board in
    (isWinningBoardHorizontally) || (isWinningBoardVertically)
;;

let mark_if_present number board =
    (Array.iter (fun line ->
        (Array.iteri
            (fun index cell ->
                if (cell.number = number)
                then (Array.set line index {number=number;isMarked=true})
            )
            line
        ))
        board
    )
;;

let sum_unmarked_cell_in_line line =
    (Array.fold_left
        (+)
        0
        (Array.map (fun cell -> if(cell.isMarked) then 0 else cell.number) line)
    )
;;

let compute_score board lastCalledNumber =
    let unmarkedSum = (Array.fold_left
        (fun sum line -> sum + (sum_unmarked_cell_in_line line))
        0
        board
    ) in
    unmarkedSum * lastCalledNumber
;;

let find_last_winning_board boardsList =
    let mark_boards boards numberDrawn =
        (List.iter (mark_if_present numberDrawn) boardsList)
    in

    let rec aux numbers lastNumberCalled boards =
        let nonWinningsBoards = (List.filter (fun board -> not (is_winning_board board)) boards) in
        if ((List.length nonWinningsBoards) = 0)
        then ((List.nth boards 0), lastNumberCalled)
        else (
            let calledNumber = (List.hd numbers) in
            (mark_boards boards calledNumber);
            (aux (List.tl numbers) calledNumber nonWinningsBoards)
        )
    in
    let (winningBoard, lastNumberCalled) = (aux numbersDrawn 12 boardsList) in
    (compute_score winningBoard lastNumberCalled)
;;

let fill_board line board lineNumber =
    (List.iteri
        (fun index number -> (Array.set (Array.get board lineNumber) index (init_cell number)))
        (List.map
            (int_of_string)
            (List.filter (fun element -> element <> "")
            (String.split_on_char ' ' line)))
    );
    board
;;

let init_board () =
    (Array.make_matrix 5 5 (init_cell 0))
;;

let rec input_lines currentBoard lineNumber boardsList =
   match try (input_line file) with End_of_file -> "EOF" with
      | "EOF" -> (find_last_winning_board (currentBoard:: boardsList))
      | "" -> (input_lines (init_board ()) 0 (currentBoard::boardsList))
      | line -> (input_lines (fill_board line currentBoard lineNumber) (lineNumber + 1) boardsList)
    ;;

(* Ignore empty line before boards *)
(input_line file);;
print_int (input_lines (init_board ()) 0 []);;