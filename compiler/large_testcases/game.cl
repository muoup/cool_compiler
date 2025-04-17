class Entity inherits IO{
    name : String <- "Entity";
    hp : Int <- 100;

    init(n : String, h : Int) : SELF_TYPE {
        {
            name <- n;
            hp <- h;
            self;
        }
    };

    take_damage(d : Int) : Int {
        hp <- if not d < hp then 0 else hp - d fi
    };

    is_alive() : Bool {
        not hp <= 0
    };

    status() : String {
        {
        out_int(hp);
        name.concat(" HP: ");
        }
    };
};

class Player inherits Entity {
    xp : Int <- 0;

    gain_xp(amount : Int) : Int {
        xp <- xp + amount
    };

    status() : String {
        {
        out_int(hp);
        out_int(xp);
        name.concat(" (Player) HP: ").concat(" XP: ");
        }
    };
};

class Enemy inherits Entity {
    power : Int <- 5;

    attack(p : Player) : Int {
        p.take_damage(power)
    };

    status() : String {
        {
        out_int(hp);
        out_int(power);
        name.concat(" (Enemy) HP: ").concat(" Power: ");
        }
    };
};

class Battle inherits IO{
    turns : Int <- 0;

    and(a : Bool, b : Bool) : Bool { 
        if a = b then 
            if a then true else false fi
        else false fi
            
    };

    fight(p : Player, e : Enemy) : String {
        {
            while and(p.is_alive(), e.is_alive()) loop {
                e.take_damage(10);
                if e.is_alive() then
                    e.attack(p)
                else
                    0
                fi;
                turns <- turns + 1;
            } pool;

            if p.is_alive() then
                p.gain_xp(20)
            else
                0
            fi;

            out_int(turns);
            ": battle ended in turns.";
        }
    };
};

class Inventory inherits IO{
    items : Int <- 0;

    add(n : Int) : Int {
        items <- items + n
    };

    remove(n : Int) : Int {
        if not n < items then items <- 0 else items <- items - n fi
    };

    summary() : String {
        {
        out_int(items);
        "Items: ";
        }
    };
};

class DecisionTree {
    choose(val : Object) : String {
        case val of
            i : Int => if i < 10 then "Low" else "High" fi;
            s : String => "Length: 4";
            b : Bool => if b then "Affirmative" else "Negative" fi;
            o : Object => "Default";
        esac
    };

    layered_logic(n : Int) : Int {
        let total : Int <- 0, i : Int <- 0 in {
            while i < n loop {
                if i / 2 = 0 then
                    total <- total + i
                else
                    total <- total - i
                fi;
                i <- i + 1;
            } pool;
            total;
        }
    };
};

class NestedScopes {
    complex_calc() : Int {
        let a : Int <- 1 in
            let b : Int <- a + 2 in
                let c : Int <- b * 3 in
                    if not c <= 10 then
                        c + a + b
                    else
                        c - a
                    fi
    };

    dispatch_test() : String {
        let e : Enemy <- new Enemy, p : Player <- new Player in {
            e.init("Goblin", 50);
            p.init("Hero", 80);

            let battle : Battle <- new Battle in {
                battle.fight(p, e);
                p.status();
            };
        }
    };
};

class FunStuff {
    factorial(n : Int) : Int {
        if n <= 1 then 1 else n * self.factorial(n - 1) fi
    };

    echo_repeat(s : String, times : Int) : String {
        let result : String <- "" in {
            while not times < 0 loop {
                result <- result.concat(s);
                times <- times - 1;
            } pool;
            result;
        }
    };
};

class Main inherits IO{
    main() : Object {
        let p : Player <- (new Player).init("Knight", 100),
            e : Enemy <- (new Enemy).init("Slime", 30),
            b : Battle <- new Battle,
            i : Inventory <- new Inventory,
            d : DecisionTree <- new DecisionTree,
            n : NestedScopes <- new NestedScopes,
            f : FunStuff <- new FunStuff in {

            out_string("== STATUS ==\n");
            out_string(p.status().concat("\n"));
            out_string(e.status().concat("\n"));

            out_string("== BATTLE ==\n");
            out_string(b.fight(p, e).concat("\n"));
            out_string(p.status().concat("\n"));

            out_string("== INVENTORY ==\n");
            i.add(3);
            i.remove(1);
            out_string(i.summary().concat("\n"));

            out_string("== DECISION TREE ==\n");
            out_string(d.choose(5).concat("\n"));
            out_string(d.choose("cool").concat("\n"));
            out_string(d.choose(false).concat("\n"));
            out_string(d.choose(new Object).concat("\n"));
            out_string("Layered logic total: "); out_int(d.layered_logic(6)); out_string("\n");

            out_string("== NESTED SCOPES ==\n");
            out_int(n.complex_calc()); out_string("\n");
            out_string(n.dispatch_test().concat("\n"));

            out_string("== FUN STUFF ==\n");
            out_string("Factorial(5): "); out_int(f.factorial(5)); out_string("\n");
            out_string(f.echo_repeat("yo", 3).concat("\n"));

            out_string("== DONE ==\n");
        }
    };
};
