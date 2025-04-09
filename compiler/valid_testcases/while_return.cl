class Main inherits IO {
    main() : Object {
        let i : Int <- 0,
            x : Object <-
            while i < 10 loop
                i <- i + 1
            pool
        in
            case x of
                o : Object => out_string("Object");
                i : Int => out_int(i);
            esac
    };
};