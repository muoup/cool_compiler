class Main inherits IO {
    main() : Object {
        {
            out_int(1 + 3);
            out_string("\n");
            out_int(1 - 3);
            out_string("\n");
            out_int(1 * 3);
            out_string("\n");
            out_int(1 / 3);
            out_string("\n");
            out_int(10 / 3);
            out_string("\n");
            out_int(3 + 10 * 4 - 2 / 5);
            out_string("\n");
            out_int(3 + 10 * (4 - 2) / 5);
            out_string("\n");
            out_int(~(1 + 3));
            out_string("\n");
            out_int(~(1 - 3));
        }
    };
};