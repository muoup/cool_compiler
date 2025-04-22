class Main inherits IO {    
    main() : Object {
        let a : Int <- 5 in
        let b : Int <- 6 in
        {
            a <- 7;
            b <- 4;
            a <- 8;
            b <- 5;
            out_int(a);
            out_string("");
            out_int(b);
        }
    };
};