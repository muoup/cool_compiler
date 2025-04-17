class Base {
    base_val : Int <- 0;

    get_val() : Int {
        base_val
    };

    set_val(v : Int) : SELF_TYPE {
        {
            base_val <- v;
            self;
        }
    };

    info() : String {
        "Base"
    };
};

class Derived1 inherits Base {
    tag : String <- "Derived1";

    get_val() : Int {
        base_val * 2
    };

    info() : String {
        tag.concat(" => ").concat("base_val")
    };
};

class Derived2 inherits Derived1 {
    counter : Int <- 5;

    get_val() : Int {
        if not counter <= 0 then
            {
                counter <- counter - 1; 
                base_val + counter;
            }
        else
            base_val
        fi
    };

    recurse_test(x : Int) : Int {
        if x <= 0 then
            0
        else
            x + self.recurse_test(x - 1)
        fi
    };
};

class Utils {
    static_sum(n : Int) : Int {
        if n <= 0 then 0 else n + self.static_sum(n - 1) fi
    };

    is_even(n : Int) : Bool {
        if n = 0 then true else self.is_odd(n - 1) fi
    };

    is_odd(n : Int) : Bool {
        if n = 0 then false else self.is_even(n - 1) fi
    };

    fancy_output(b : Bool, i : Int) : String {
        if b then
            "Even: ".concat("i")
        else
            "Odd: ".concat("i")
        fi
    };

    test_case(val : Object) : Int {
        case val of
            i : Int => i + 1;
            s : String => s.length();
            b : Bool => if b then 1 else 0 fi;
            o : Object => 0;
        esac
    };
};

class Main inherits IO{
    main() : Object {
        let b : Base <- new Base,
            d1 : Derived1 <- new Derived1,
            d2 : Derived2 <- new Derived2,
            u : Utils <- new Utils,
            result : Int,
            str : String in {

            b.set_val(10);
            d1.set_val(20);
            d2.set_val(30);

            out_string("Calling info on all:\n");
            out_string(b.info().concat("\n"));
            out_string(d1.info().concat("\n"));
            out_string(d2.info().concat("\n"));

            out_string("Values:\n");
            out_int(b.get_val()); out_string("\n");
            out_int(d1.get_val()); out_string("\n");
            out_int(d2.get_val()); out_string("\n");

            out_string("Derived2 recursive test (sum 0 to 4):\n");
            out_int(d2.recurse_test(4));
            out_string("\n");

            out_string("Static sum via Utils:\n");
            out_int(u.static_sum(5));
            out_string("\n");

            out_string("Even/Odd checks:\n");
            let i : Int <- 7 in {
                out_string(u.fancy_output(u.is_even(i), i).concat("\n"));
                out_string(u.fancy_output(u.is_even(i + 1), i + 1).concat("\n"));
            };

            out_string("Case test with various types:\n");
            out_int(u.test_case(10)); out_string("\n");
            out_int(u.test_case("hello")); out_string("\n");
            out_int(u.test_case(true)); out_string("\n");
            out_int(u.test_case(new Object)); out_string("\n");

            out_string("Nested lets and expressions:\n");
            let a : Int <- 2, b : Int <- 3, c : Int in {
                c <- let x : Int <- a * b, y : Int <- x + 1 in x + y;
                out_int(c); out_string("\n");
            };

            out_string("Done!\n");
        }
    };
};
