class Main inherits IO {
    main() : Object {
        {
            out_string("10 / 4: ");
            out_int(10 / 4);
            out_string("\n");

            out_string("10 * 4: ");
            out_int(10 * 4);
            out_string("\n");

            out_string("10 + 4: ");
            out_int(10 + 4);
            out_string("\n");

            out_string("10 - 4: ");
            out_int(10 - 4);
            out_string("\n");

            if (10 < 4) then
                out_string("10 < 4\n")
            else
                out_string("10 >= 4\n")
            fi;

            if not (10 <= 4) then
                out_string("10 > 4\n")
            else
                out_string("10 <= 4\n")
            fi;

            if 10 = 4 then
                out_string("10 = 4\n")
            else
                out_string("10 != 4\n")
            fi;

            if 4 = 4 then
                out_string("4 = 4\n")
            else
                out_string("4 != 4\n")
            fi;
        }
    };
};