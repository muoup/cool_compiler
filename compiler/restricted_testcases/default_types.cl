class Main inherits IO {
    main() : Object {
        let default_int : Int in
        let default_bool : Bool in
        let default_object : Object in

        if not (default_int = 0) then
            out_string("default_int failed!\n")
        else if not (default_bool = false) then
            out_string("default_bool failed!\n")
            out_string("success!\n")
        fi fi
    };
};