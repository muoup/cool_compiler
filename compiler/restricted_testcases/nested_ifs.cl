class Main inherits IO {
    main() : Object {
        {
            if true then
                if false then
                    abort()
                else
                    out_string("Correct!")
                fi
            else
                abort()
            fi;

            out_string("Exited correctly!");
        }
    };
};