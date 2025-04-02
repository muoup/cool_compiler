class Main inherits IO {
    main() : Object {
        let default_int : Int in
        let default_bool : Bool in
        let default_string : String in
        let default_object : Object in

        if not (default_int = 0) then
            out_string("default_int failed!\n")
        else if not (default_bool = false) then
            out_string("default_bool failed!\n")
        else if not (default_string = "") then
            out_string("default_string failed!\n")
        else if not (isvoid default_object) then
            out_string("default_object failed!\n")
        else
            out_string("success!\n")
        fi fi fi fi
    };
};