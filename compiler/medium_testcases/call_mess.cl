class Main inherits IO {
    i : Int;
    
    init(n : Int) : SELF_TYPE {
        {
            i <- n;
            self;
        }
    };

    getI() : Int {
        i
    };

    printI() : Object {
        out_int(i)
    };

    id(m : Main) : SELF_TYPE {
        (new SELF_TYPE)
            .init(i + m.getI() + 1)
    };

    trio(m1 : Main, m2 : Main, m3 : Main) : Main {
        let x : Int <-
            m1.getI() + m2.getI() + m2.id(m1).getI()
        in
        m2.out_int(x)
    };

    main() : Object {
        trio((new Main).init(1), (new Main).init(2), (new Main).init(3))
            .printI()
    };
};