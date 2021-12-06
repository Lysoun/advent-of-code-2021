open Printf

let input = "input.txt";;
let init_lantern_fishes_by_internal_timer () = (Array.make 9 0);;

let fill_fishes_by_internal_timer_with_initial_state lanternfishesByInternalTimer initialState =
    (List.iter
        (fun internalTimer ->
            (Array.set
                lanternfishesByInternalTimer
                internalTimer
                ((Array.get lanternfishesByInternalTimer internalTimer) + 1)))
        (List.map (int_of_string) (String.split_on_char ',' initialState)))
;;
let lanternFishesByInternalTimer = (init_lantern_fishes_by_internal_timer ());;
(fill_fishes_by_internal_timer_with_initial_state lanternFishesByInternalTimer (input_line (open_in input)));;

let count_lanternfishes_number lanternfishesByInternalTimer =
    (Array.fold_left (+) 0 lanternfishesByInternalTimer)
;;

let rec range a b =
    if(b < a)
    then (List.rev (range b a))
    else (List.init (b - a + 1) (fun index -> a + index))
;;

let rangeTo8 = (range 0 8);;

let decrease_internal_timer lanternfishesByInternalTimer =
    let nextDayLanternfishesByInternalTimer = (init_lantern_fishes_by_internal_timer ()) in
    (List.iter
        (fun index->

            (Array.set
                nextDayLanternfishesByInternalTimer
                index
                (Array.get
                    lanternfishesByInternalTimer
                    ((index + 1) mod 9))
        ))
        rangeTo8);

    (Array.set
        nextDayLanternfishesByInternalTimer
        6
        ((Array.get
            lanternfishesByInternalTimer
            0) + (Array.get
                             nextDayLanternfishesByInternalTimer
                             6)));
    nextDayLanternfishesByInternalTimer
;;

let maxDay = 256;;

let rec reproduceLanternFishesUntilDay lanternfishesByInternalTimer day =
    if(day = 0)
    then (count_lanternfishes_number lanternfishesByInternalTimer)
    else
        (reproduceLanternFishesUntilDay (decrease_internal_timer lanternfishesByInternalTimer) (day - 1))
;;

(print_int (reproduceLanternFishesUntilDay lanternFishesByInternalTimer maxDay));;