class Main inherits IO {
    counter : Int;
    counter2 : Int;

    gen_number2() : Int {
        {
            counter2 <- counter2 + 1;
            counter2 <- counter2 * 2;
            counter2;
        }
    };
    
    gen_number() : Int {
        {
            counter <- counter + 1;
            counter + gen_number2();
        }
    };

    main() : Object {
        let i : Int <- (
            if not (2 < 1) then gen_number() else 1 + gen_number() fi +
            if 3 < 4 then 1 + gen_number() else gen_number() fi +
            if 5 = 5 then 5 + gen_number() else 7 + gen_number() fi +
            if not (6 < 7) then 6 + gen_number() else 8 + gen_number() fi
        ) in

        out_int(i)
    };
};