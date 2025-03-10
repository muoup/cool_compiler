class Base {
    gen_self() : SELF_TYPE {
        new SELF_TYPE
    };
};

class Derived inherits Base {};

class Main inherits IO {
    main() : Object {
        {
            out_string((new Derived).gen_self().type_name());
            out_string("\n");
        }
    };
};