class Main inherits IO {
    say(o : Object) : Object {
        if isvoid o then
            out_string("void")
        else
            out_string(o.type_name())
        fi
    };

    main() : Object {
        say(
            while false loop
                out_string("looping")
            pool
        )
    };
};