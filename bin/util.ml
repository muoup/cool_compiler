let rec ntail (lst : 'a list) (n : int) =
    match n with
    | 0 -> lst
    | n -> ntail (List.tl lst) (n - 1)

let error_and_exit line_number message = 
    Printf.printf "ERROR: %i: Type-Check: %s\n" line_number message;
    exit 0;