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
            case
                if isvoid out_string("Hello, world!") then
                    self
                else
                    0
                fi
            of
                i : Int => i;
                o : Object => { abort(); 0; };
            esac
                .copy()
                .type_name()
                .length()

            +

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