class Base {
    base() : Object {1};
};

class Derived inherits Base {
    derived() : Object {2};
};


class Child inherits Derived {
    child() : Object {3};
};

class Unrelated {};

class Main {
	main() : Object { 
        let b : Base <- new Base in
        let d : Derived <- new Derived in
        let c : Child <- new Child in
        c.child()
     };
};
