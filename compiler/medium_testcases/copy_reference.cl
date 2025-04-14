class Data {
    a : Int;

    set(x : Int) : Object {
        a <- x
    };

    get() : Int {
        a
    };
};

class Container {
    d : Data <- new Data;

    set(x : Int) : SELF_TYPE {
        {
            d.set(x);
            self;
        }
    };

    get() : Int {
        d.get()
    };
};

class Main inherits IO {
    main() : Object {
        let c1 : Container <- (new Container).set(1),
            c2 : Container <- c1.copy()
        in
        {
            c2.set(2);

            out_int(c1.get());
            out_int(c2.get());
        }
    };
};