class Base {};

class Derived inherits Base {};

class Main inherits IO {
    main() : Object {
        case (new Derived) of
            s : String => out_string("String");
            o : Base => out_string("Base");
        esac
    };
};