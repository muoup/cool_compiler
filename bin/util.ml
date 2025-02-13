let rec ntail (lst : 'a list) (n : int) =
    match n with
    | 0 -> lst
    | n -> ntail (List.tl lst) (n - 1)
