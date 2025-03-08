class Main {
    main() : Object {
        let data1 : Int <- 1, data2 : Int <- data1 in
        {
            data1 <- 2;

            if data1 = data2 then
                abort()
            else
                0
            fi;   
        }
    };
};