class Root inherits IO {
    id : Int <- 0;
    name : String <- "Root";

    init(i : Int, n : String) : SELF_TYPE {
        {
            id <- i;
            name <- n;
            self;
        }
    };

    info() : String { 
        {
        out_int(id);
        name.concat(" [").concat("]");
        }
    };

    get_id() : Int { id };

    compute(x : Int) : Int {
        if x <= 1 then 1 else x * self.compute(x - 1) fi
    };
};

class Level1 inherits Root {
    val : Bool <- false;

    info() : String { 
        {
        out_int(id);
        "L1->".concat(name).concat(":");
        }
    };

    toggle() : Bool {
        val <- not val
    };
};

class Level2 inherits Level1 {
    tag : String <- "Level2";

    compute(x : Int) : Int {
        let res : Int <- 0 in {
            while not x <= 0 loop {
                res <- res + x;
                x <- x - 1;
            } pool;
            res;
        }
    };

    concat_tag() : String {
        tag.concat("-").concat(name)
    };
};

class Thing {
    val : Int <- 5;

    bump(n : Int) : Int {
        val <- val + n
    };

    to_str() : String {
        "val: "
    };
};

class Counter {
    count : Int <- 0;

    inc() : Int {
        count <- count + 1
    };

    reset() : Int {
        count <- 0
    };

    times(n : Int) : Int {
        let i : Int <- 0 in {
            while i < n loop {
                self.inc();
                i <- i + 1;
            } pool;
            count;
        }
    };
};

class Util {
    fib(n : Int) : Int {
        if n <= 1 then 1 else self.fib(n - 1) + self.fib(n - 2) fi
    };

    describe(val : Object) : String {
        case val of
            i : Int => "Int: i";
            s : String => "Str: ".concat(s);
            b : Bool => if b then "Bool: true" else "Bool: false" fi;
            o : Object => "Generic Object";
        esac
    };

    crazy_let(x : Int) : Int {
        let x : Int <- x + 1, y : Int <- x * 2, z : Int in {
            z <- x + y;
            z * 2;
        }
    };

    shadow_demo() : Int {
        let a : Int <- 5 in 
            let a : Int <- a + 2 in
                a * a
    };
};

class Nested {
    nested_exprs() : Int {
        let a : Int <- 1, b : Int <- 2, c : Int <- 3 in
            if (a + b) < c then
                c * (a + b)
            else
                (let x : Int <- a * b, y : Int <- x + c in x + y)
            fi
    };

    loop_expr() : Int {
        let sum : Int <- 0, i : Int <- 0 in {
            while i < 10 loop {
                sum <- sum + i;
                i <- i + 1;
            } pool;
            sum;
        }
    };
};

class Main inherits IO{
    main() : Object {
        let r : Root <- (new Root).init(1, "Base"),
            l1 : Level1 <- (new Level1).init(2, "L1"),
            l2 : Level2 <- (new Level2).init(3, "L2"),
            u : Util <- new Util,
            t : Thing <- new Thing,
            c : Counter <- new Counter,
            n : Nested <- new Nested in {

            out_string("== Root Info ==\n");
            out_string(r.info().concat("\n"));
            out_string("Compute (Root): "); 
            out_int(r.compute(4)); 
            out_string("\n");

            out_string("== Level1 Info ==\n");
            out_string(l1.info().concat("\n"));
            out_string("Toggle val: "); 
            l1.toggle(); 
            out_string("\n");

            out_string("== Level2 Info ==\n");
            out_string(l2.info().concat("\n"));
            out_string("Compute (Level2): "); 
            out_int(l2.compute(4)); 
            out_string("\n");
            out_string("Concat tag: "); 
            out_string(l2.concat_tag()); 
            out_string("\n");

            out_string("== Thing ==\n");
            out_string("Initial val: "); 
            out_string(t.to_str());
            out_string("\n");
            t.bump(7);
            out_string("Bumped val: "); 
            out_string(t.to_str()); 
            out_string("\n");

            out_string("== Counter ==\n");
            c.reset();
            out_string("Counting to 5: "); out_int(c.times(5)); out_string("\n");

            out_string("== Util ==\n");
            out_string("Fib(5): "); out_int(u.fib(5)); out_string("\n");
            out_string(u.describe(42).concat("\n"));
            out_string(u.describe("hi!").concat("\n"));
            out_string(u.describe(true).concat("\n"));
            out_string(u.describe(new Object).concat("\n"));
            out_string("Crazy let: "); out_int(u.crazy_let(2)); out_string("\n");
            out_string("Shadow demo: "); out_int(u.shadow_demo()); out_string("\n");

            out_string("== Nested ==\n");
            out_string("Nested exprs: "); out_int(n.nested_exprs()); out_string("\n");
            out_string("Loop exprs: "); out_int(n.loop_expr()); out_string("\n");

            out_string("== Done ==\n");
        }
    };
};
