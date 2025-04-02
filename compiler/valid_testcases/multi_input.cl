class Main inherits IO {
    main() : Object {
        let str1 : String <- in_string() in
        let str2 : String <- in_string() in
        {
            out_string(str1);
            out_string(str2);
        }
    };
};