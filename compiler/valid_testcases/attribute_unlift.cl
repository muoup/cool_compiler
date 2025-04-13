class Main inherits IO {
    o : Object <- 1;

    main() : Object {
        {   
            out_string(o.type_name());

            o <- 2;
            o.abort();
        }
    };
};