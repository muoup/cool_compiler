class Main inherits IO {
    say_type(o : Object) : Object {
        out_string(o.type_name())
    };

    main() : Object {
        {
            say_type(2);
            self.say_type(2);
            self@Main.say_type(2);
        }
    };
};