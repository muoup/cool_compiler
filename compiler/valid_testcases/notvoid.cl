class Main inherits IO {
    main() : Object {
        let obj : Object <- new Object in

        if isvoid obj then
            out_string("ERROR\n")
        else
            out_string("new Object is not void")
        fi
    };
};