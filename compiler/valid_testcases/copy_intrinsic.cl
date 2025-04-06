class Main inherits IO {
    main() : Object {
        let i : Int <- new Int in
        let j : Int <- 3 in
        let k : Int <- i.copy() + j.copy() in

        out_int(k)
    };
};