class A inherits IO {
    x : Int <- 0;
    y : String <- "Hello";

    init() : SELF_TYPE {
        {
            x <- 10;
            y <- y.concat(" World!");
            self;
        }
    };

    add(a : Int, b : Int) : Int {
        a + b
    };

    lp(n : Int) : Int {
        let acc : Int <- 0 in {
            while acc < n loop {
                acc <- acc + 1;
            } pool;
            acc;
        }
    };

    describe() : String {
        if x < 10 then
            "Small"
        else
            if x = 10 then "Equal" else "Big" fi
        fi
    };
};

class B inherits A {
    z : Bool <- false;

    describe() : String {
        if z then
            "Z is true"
        else
            (new A).describe()
        fi
    };

    toggleZ() : Bool {
        z <- not z
    };

    callLp(n : Int) : Int {
        self.lp(n)
    };
};

class C inherits B {
    dummy : Object;

    compute(n : Object) : Int {
        case n of
            a : Int => a + 1;
            b : Bool => if b then 1 else 0 fi;
            c : String => c.length();
            d : Object => 42;
        esac
    };

    chainOps() : Int {
        let a : Int <- 1, b : Int <- 2, c : Int <- 3 in
            (a + b) * c - (b / a)
    };

    printAll() : Object {
        {
            out_string("Values:\n");
            out_int(self.lp(5));
            out_string("\nDesc: ");
            out_string(self.describe());
        }
    };
};

class Main inherits IO{
    main() : Object {
        let a : A <- new A,
            b : B <- new B,
            c : C <- new C in {
            a.init();
            b.init();
            c.init();

            out_string("Testing describe:\n");
            out_string(a.describe().concat("\n"));
            out_string(b.describe().concat("\n"));
            out_string(c.describe().concat("\n"));

            out_string("Toggling z:\n");
            b.toggleZ();
            out_string(b.describe().concat("\n"));

            out_string("Calling lp:\n");
            out_int(b.callLp(3));
            out_string("\n");

            out_string("Chained ops:\n");
            out_int(c.chainOps());
            out_string("\n");

            out_string("Case expressions:\n");
            out_int(c.compute(10));
            out_string("\n");
            out_int(c.compute(true));
            out_string("\n");
            out_int(c.compute("hello"));
            out_string("\n");
            out_int(c.compute(new Object));
            out_string("\n");

            out_string("Calling printAll:\n");
            c.printAll();

            out_string("\nDone.\n");
        }
    };
};
