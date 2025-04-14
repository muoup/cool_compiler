class Main inherits IO {
    main() : Object {
        let s1 : Object <- "Hello, world!",
            s2 : Object <- in_string()
        in
            if s1 = s2 then
                out_string("Strings are equal\n")
            else
                out_string("Strings are not equal\n")
            fi
    };
};