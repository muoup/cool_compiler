class Main inherits IO {
    main() : Object {
        let s1 : Object <- "Hello, world!",
            s2 : Object <- "Hello, world!",
            i1 : Object <- 1,
            i2 : Object <- 1,
            b1 : Object <- true,
            b2 : Object <- true
        in
        {
            if s1 = s2 then
                out_string("s1 == s2\n")
            else
                out_string("s1 != s2\n")
            fi;

            if i1 = i2 then
                out_string("i1 == i2\n")
            else
                out_string("i1 != i2\n")
            fi;

            if b1 = b2 then
                out_string("b1 == b2\n")
            else
                out_string("b1 != b2\n")
            fi;
        }
    };
};