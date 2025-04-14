class Main inherits IO {
    i : Int;
    o : Object;

    main() : Object {
        {
            i <- (o <- 2);
            out_int(i);
        }
    };
};