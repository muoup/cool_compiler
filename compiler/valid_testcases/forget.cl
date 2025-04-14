class Main inherits IO {
    get() : Main {
        {
            out_string("Hello, world!");
            self;
        }
    };

    drop(m : Main) : Main {
        let a : Main in
        a
    };

    main() : Object {
        get().drop( self ).drop( get() )
    };
};