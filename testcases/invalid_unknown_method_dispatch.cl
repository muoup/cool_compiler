class Animal {};

class Cow inherits Animal {
    moo() : String {
        "moo"
    };
};

class Blerg {
    foo() : Object {
        let cow : Animal <- new Cow in cow.moo()
    };
};

class Main inherits Blerg {
    main() : Object {
        2
    };
};