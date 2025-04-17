class Organism inherits IO{
    id : Int <- 0;
    alive : Bool <- true;

    init(i : Int) : SELF_TYPE {
        {
            id <- i;
            self;
        }
    };

    die() : Bool {
        alive <- false
    };

    revive() : Bool {
        alive <- true
    };

    is_alive() : Bool {
        alive
    };

    status() : String { 
        {
        out_int(id);
        "Organism #".concat(" Alive: ").concat(if alive then "Yes" else "No" fi);
        }
    };
};

class Creature inherits Organism {
    energy : Int <- 10;

    feed(amount : Int) : Int {
        energy <- energy + amount
    };

    expend(amount : Int) : Int {
        if not amount < energy then
            energy <- 0
        else
            energy <- energy - amount
        fi
    };

    status() : String {
        {
        out_int(id);
        out_int(energy);
        "Creature #".concat(" Energy: ").concat(" Alive: ").concat(if alive then "Yes" else "No" fi);
        }
    };
};

class Bot inherits Creature {
    version : String <- "1.0";

    upgrade(v : String) : String {
        version <- v
    };

    status() : String {
        {
        out_int(id);
        out_int(energy);
        "Bot #".concat(" v").concat(version).concat(" Energy: ").concat(" Alive: ").concat(if alive then "Yes" else "No" fi);
        }
    };
};

class World inherits IO{
    tick : Int <- 0;

    simulate(o : Organism, rounds : Int) : String {
        {
            while tick < rounds loop {
                if o.is_alive() then
                    o.die()
                else
                    o.revive()
                fi;
                tick <- tick + 1;
            } pool;
            out_int(tick);
            "Simulation ended at tick ";
        }
    };
};

class MathUtils {
    power(base : Int, exp : Int) : Int {
        if exp <= 0 then 1 else base * self.power(base, exp - 1) fi
    };

    avg(a : Int, b : Int) : Int {
        (a + b) / 2
    };

    sum_until(n : Int) : Int {
        let sum : Int <- 0, i : Int <- 0 in {
            while i <= n loop {
                sum <- sum + i;
                i <- i + 1;
            } pool;
            sum;
        }
    };
};

class Logic {
    analyze(x : Object) : String {
        case x of
            i : Int => if i < 0 then "Negative" else "Positive or Zero" fi;
            s : String => "String of length s";
            b : Bool => if b then "True" else "False" fi;
            o : Object => "Unknown type";
        esac
    };

    deep_let(n : Int) : Int {
        let a : Int <- n * 2 in
            let b : Int <- a + 3 in
                let c : Int <- b * b in
                    c + b + a
            
    };
};

class Manager inherits IO {
    org : Organism <- new Organism;
    cr : Creature <- new Creature;
    bt : Bot <- new Bot;
    math : MathUtils <- new MathUtils;
    logic : Logic <- new Logic;

    init_all() : SELF_TYPE {
        {
            org.init(1);
            cr.init(2);
            bt.init(3);
            bt.upgrade("2.5");
            self;
        }
    };

    run_all() : Object {
        {
            out_string(org.status().concat("\n"));
            out_string(cr.status().concat("\n"));
            out_string(bt.status().concat("\n"));

            cr.feed(5);
            bt.expend(4);
            out_string("After energy change:\n");
            out_string(cr.status().concat("\n"));
            out_string(bt.status().concat("\n"));

            out_string("MathUtils sum_until(5): "); out_int(math.sum_until(5)); out_string("\n");
            out_string("MathUtils power(2, 5): "); out_int(math.power(2, 5)); out_string("\n");

            out_string("Logic deep let: "); out_int(logic.deep_let(3)); out_string("\n");
            out_string("Logic analyze true: "); out_string(logic.analyze(true).concat("\n"));
            out_string("Logic analyze \"hello\": "); out_string(logic.analyze("hello").concat("\n"));
            out_string("Logic analyze 42: "); out_string(logic.analyze(42).concat("\n"));

            self;
        }
    };
};

class Main inherits IO {
    main() : Object {
        let w : World <- new World,
            m : Manager <- new Manager in {
            
            out_string("=== INIT ===\n");
            m.init_all();

            out_string("=== RUN ===\n");
            m.run_all();

            out_string("=== SIMULATE ===\n");
            out_string(w.simulate(new Bot, 4).concat("\n"));

            out_string("=== DONE ===\n");
        }
    };
};
