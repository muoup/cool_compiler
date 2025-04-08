class Main inherits IO {
    main() : Object {
        let x : String <- "Hello, world!",
            y : String <- "Bye"
        in
            out_string(
                y.concat(x.substr(5, 8))
            )
    };
};