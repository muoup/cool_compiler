class Main inherits IO {
    main() : Object {
        let i : Int <- 0 in
        {
            while i < 25 loop
            {
                out_int(i);
                out_string(" || ");

                i <- i + 1;
            }
            pool;

            out_int(i);
        }
    };
};