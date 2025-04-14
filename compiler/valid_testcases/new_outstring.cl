class Main {
    main() : Object {
        let x : String,
            y : String in
        {
            x <- (new IO).in_string();
            (new IO).out_string("Hello, world");
            y <- x;
            (new IO).out_string(y);
            (new IO).out_string("Hello, world");
        }
    };
};