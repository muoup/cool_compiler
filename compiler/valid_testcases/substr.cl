class Main inherits IO {
    main() : Object {
        let str : String <- "Hello, World!" in
        let substr : String <- str.substr(7, 5) in
        
        out_string(substr)
    };
};