class Base {};

class Derived inherits Base {};

class Main inherits IO {
    main() : Object {
        let x : Base <- new Derived  in
        case (x) of
            s : String => out_string("String");
            i : Int => out_int(1);
            b : Base => out_string("Base");
            d : Derived => out_string("Derived");
            o : Object => out_string("Object");
        esac
    };
};