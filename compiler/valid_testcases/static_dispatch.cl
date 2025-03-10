class Base {
    talk(io : IO) : Object {
        io.out_string("Base\n")
    };
};

class Derived inherits IO {
    talk(io : IO) : Object {
        io.out_string("Derived\n")
    };
};

class Main inherits IO {
    main() : Object {
        let base : Base <- new Derived in
        base@Base.talk(self) 
    };
};