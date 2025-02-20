
let error_and_exit line_number message = 
    Printf.printf "ERROR: %i: Type-Check: %s\n" line_number message;
    exit 0;
