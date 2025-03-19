class Main inherits IO {
    main() : Object {
        let a : Int <- 2 in
        {
            let a : Int <- 3 in
            {
                out_int(a);
                out_string("\n");
            };

            out_int(a);
            out_string("\n");
        }
    };
};