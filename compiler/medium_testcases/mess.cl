class Main inherits IO {
    o() : Object {
        2
    };

    unwrap(o : Object) : Int {
        case o of
            i : Int => i;
        esac
    };

    wrap(o : Int) : Object {
        o
    };

    main() : Object {
        out_int(
            o()
                .copy()
                .type_name()
                .length()

            +

            unwrap(
                self.o()
                    .copy()
            )

            +

            wrap(
                unwrap(
                    self@Main.o()
                        .copy()
                )
            )
                .type_name()
                .length()

            +

            case out_string("Hello, world!") of
                i : Int => i;
                o : Object => self;
            esac
                .copy()
                .type_name()
                .length()
        )       
    };
};