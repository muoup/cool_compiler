class Main inherits IO {
    main() : Object {
        let i : Int <- 0 in

        while i < 100 loop
        {
            i <- i + 1;
            if not (i < 50) then
                if (i / 3 * 3) = i then
                {
                    out_int(i);
                    out_string("\n");
                }
                else 
                    0
                fi
            else 
                0
            fi;
        }
        pool
    };
};