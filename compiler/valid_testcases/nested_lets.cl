class Main inherits IO {
    main() : Object {
        let a : Int in
        let b : Int in
        {
            a <- 2;
            
            let b : Int <- 3 in
            out_int(a + b);
            
            b <- 4;

            out_int(a + b);
        }
    };
};