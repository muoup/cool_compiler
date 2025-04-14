class Main inherits IO {
    get() : Main {
        {
            out_string("Hello, world!");
            self;
        }
    };

    drop(m : Main) : Main {
        let o : String in
        m.get()
    };

    main() : Object {
        get().drop( get() )
    };
};