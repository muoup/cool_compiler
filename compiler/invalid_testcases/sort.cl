class List {
    elem : Int;
    tl : List;

    append(i : Int) : Object {
        if isvoid tl then
        {
            elem <- i;
            tl <- new List;
        }
        else
            tl.append(i)
        fi
    };

    next() : List {
        tl
    };

    elem() : Int {
        elem
    };

    set(ii : Int) : Object {
        elem <- ii
    };

    sort() : Object {
        if isvoid tl then
            0
        else 
        {
            tl.sort();

            while not (elem() < next().elem()) loop
            {
                let temp : Int <- elem() in
                {
                    elem <- next().elem();
                    next().set(temp);
                };
                tl.sort();
            }
            pool;
        }
        fi
    };

    print(io : IO) : Object {
        {
            io.out_int(elem);
            io.out_string("\n");

            if isvoid tl then
                0
            else
                tl.print(io)
            fi;
        }
    };
};

class Main inherits IO {
    main() : Object {
        let lst : List <- new List in
        {
            lst.append(4);
            lst.append(2);
            lst.append(5);
            lst.append(1);
            lst.append(3);

            lst.sort();
            lst.print(self);  
        }
    };
};