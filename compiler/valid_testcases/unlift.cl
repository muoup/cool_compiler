class Main inherits IO {
    main() : Object {
        let x : Object <- 2 in
        let y : Object <- false in
        let z : Object <- "String" in

        let x2 : Object, y2 : Object, z2 : Object in
        {
            x2 <- 2;
            y2 <- false;
            z2 <- "String";

            out_string(x.type_name());
            out_string(y.type_name());
            out_string(z.type_name());

            out_string(x2.type_name());
            out_string(y2.type_name());
            out_string(z2.type_name());
        }
    };
};