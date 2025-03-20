class Main inherits IO {
    main() : Object {
        let x : Int <- 2 in
        let y : Int <- 4 in
        {
            x <- x * y;
            y <- y / 2;

            while x < 100 loop
            {
                x <- 10 * x;
                y <- y + ~x * y;

                while not (y < 100) loop
                {
                    y <- y / 2;
                    x <- x - 10;
                }
                pool;
            }
            pool;

            x <- x + x * y / x;
            
            if x < y then
            {
                out_int(x);
                out_int(y);
            }
            else
            {
                out_int(y);
                out_int(x);
            }
            fi;
        }
    };
};