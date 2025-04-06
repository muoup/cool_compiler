class Main inherits IO {
    main() : Object {
        let str1 : String <- in_string(),
            str2 : String <- in_string() in
        
        if (str1 = str2) then
            out_string("Strings are equal\n")
        else
            out_string("Strings are not equal\n")
        fi
    };
};