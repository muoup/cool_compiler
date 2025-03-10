class Base {
    talk(io : IO, i : Int) : Object {
        abort()
    };
};

class Derived {
    talk(io: IO, i : Int) : Object {
        if i = 2 then
            talk(io, i - 1)
        else
            io.out_int(i)
        fi
    };
};

class Main inherits IO {
    main() : Object {
        {
            (new Derived).talk(self, 2);
            out_string("\n");
        }
    };
};