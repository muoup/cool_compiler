class A {
    print(io : IO) : Object {
        io.out_string("A\n")
    };
};

class B inherits A {
    print(io : IO) : Object {
        io.out_string("B\n")
    };
};

class Main inherits IO {
    main() : Object {
        let a : A <- new A,
            b : B <- new B in
        {
            a.print(self);
            b.print(self);
            out_string("\n");
        }
    };
};