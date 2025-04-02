class Main inherits IO {
    main() : Object {
        let b : Bool in
        let i : Int in
        {
            if b then
                out_string("true\n")
            else
                out_string("false\n")
            fi;

            while i < 100
            loop
            {
                if i / 2 * 2 = i then
                    b <- true
                else
                    b <- false
                fi;

                if b then
                    out_string("even\n")
                else
                    out_string("odd\n")
                fi;

                i <- i + 1;
            }
            pool;   
        }
    };
};