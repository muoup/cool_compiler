class Main inherits IO {
    main() : Object {
        {
            if 1 < 2 then
                out_string("1 < 2\n")
            else
                out_string("1 >= 2\n")
            fi;

            if 2 < 1 then
                out_string("2 < 1\n")
            else
                out_string("2 >= 1\n")
            fi;

            if not (1 < 2) then
                out_string("not (1 < 2)\n")
            else
                out_string("not (1 >= 2)\n")
            fi;

            if not (2 < 1) then
                out_string("not (2 < 1)\n")
            else
                out_string("not (2 >= 1)\n")
            fi;

            if 1 = 2 then
                out_string("1 = 2\n")
            else
                out_string("1 != 2\n")
            fi;

            if not (1 = 2) then
                out_string("not (1 = 2)\n")
            else
                out_string("not (1 != 2)\n")
            fi;
        }
    };
};