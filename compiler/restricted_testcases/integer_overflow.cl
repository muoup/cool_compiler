class Main inherits IO {
    main() : Object {
        let i : Int <- ~2147483647 - 2 in 
        {
            out_int(i);
            out_int(i + 2);
        }
    };
};