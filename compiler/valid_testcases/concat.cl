class Main inherits IO {
    main() : Object {
        let str1 : String <- "Hello, " in
        let str2 : String <- "World!" in

        out_string(str1.concat(str2))
    };
};