class Base {};
class Derived inherits Base {};

class Main inherits IO {
    main() : Object {
        let b : Base <- new Derived in

        case b of
            b : Base => out_string("Base");
            d : Derived => out_string("Derived");
        esac
    };
};