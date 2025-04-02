class Main inherits IO {
    main() : Object {
        let i : Int <- {
            if true then
                2 + 2
            else
                2 - 2
            fi;
        } in
        out_int(i)
    };
};