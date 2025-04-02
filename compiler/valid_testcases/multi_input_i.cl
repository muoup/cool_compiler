class Main inherits IO {
    main() : Object {
        let str1 : Int <- in_int() in
        {
            out_int(str1);
            let str2 : Int <- in_int() in
            {
                out_int(str1 + str2);
            };
        }
    };
};