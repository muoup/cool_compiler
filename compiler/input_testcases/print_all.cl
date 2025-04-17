class Main inherits IO {
    main() : Object {
        let buff : String <- in_string() in

        while not (buff = "") loop
        {
            out_string(buff);
            out_int(buff.length());
            out_string("\n");
            buff <- in_string();
        }
        pool
    };
};