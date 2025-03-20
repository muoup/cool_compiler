class Main inherits IO {
    main() : Object {
        let data1 : Int <- 1, data2 : Int <- data1 in
        {
            data1 <- 2;

            if data1 = data2 then
                0
            else
                out_string("Correct!\n")
            fi;   
        }
    };
};