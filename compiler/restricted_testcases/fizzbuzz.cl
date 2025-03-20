class Main inherits IO {
    main() : Object {
        let x : Int <- 0 in
        while x < 100 loop
        {
            x <- x + 1;

            if (x / 3 * 3) = x then
                out_string("Fizz")
            else
                0
            fi;

            if (x / 5 * 5) = x then
                out_string("Buzz")
            else
                0
            fi;

            out_string("\n");
        }
        pool
    };
};