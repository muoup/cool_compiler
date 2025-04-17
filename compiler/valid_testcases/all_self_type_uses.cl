class Base inherits IO {
    init() : SELF_TYPE {
        let x : SELF_TYPE <- new SELF_TYPE in
        {
            x.talk();
            x;
        }
    };

    talk() : Object {
        out_string("Base\n")
    };
};

class Derived inherits Base {
    talk() : Object {
        out_string("Derived\n")
    };
};

class Main inherits IO {
    main() : Object {
        {
            (new Base).init();
            (new Derived).init();
        }
    };
};