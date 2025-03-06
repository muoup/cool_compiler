class A {
    x : String;
    foo (x : Int) : Object {
        while true loop x pool
    };
};

class C inherits B {
    bar() : Object{
        self
    };
};

class B inherits A  {
    foo(k : Int) : Object {
        self@C.bar()
    };
};

class Main {
    main() : Object {
        0
    };
};