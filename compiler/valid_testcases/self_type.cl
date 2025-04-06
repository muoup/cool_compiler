class Base {
    new_from() : SELF_TYPE {
        new SELF_TYPE;
    };
};

class Derived inherits Base {};

class Main {
    main() : Object {
        out_string(
            (new Derived).new_from().type_name()
        )
    };
};