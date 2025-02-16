let rec ntail (lst : 'a list) (n : int) =
    match n with
    | 0 -> lst
    | n -> ntail (List.tl lst) (n - 1)

let print_error line_number message = 
    Printf.printf "ERROR: %i: Type-Check: %s\n" line_number message;