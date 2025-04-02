class Main inherits IO {
    main() : Object {
        let str1 : String <- in_string() in
        let i1 : Int <- in_int() in
        {
            out_string(str1);
            out_int(i1);
        }
    };
};