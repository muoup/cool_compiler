class IntList {
    head : Object;
    tail : IntList;

    insert(i : Int) : Object {
        if (isvoid head) then
        {
            head <- i;
            tail <- new IntList;
        }
        else
            tail.insert(i)
        fi
    };

    print(io : IO) : Object {
        if (isvoid head) then
            0
        else
        {
            case head of
                i : Int => io.out_int(i);
            esac;
            
            io.out_string(" ");
            tail.print(io);
        }
        fi
    };
};

class Main inherits IO {
    main() : Object {
        let list : IntList <- new IntList in
        {
            list.insert(1);
            list.print(self);
            out_string("\n");

            list.insert(2);
            list.print(self);
            out_string("\n");

            list.insert(3);
            list.print(self);
            out_string("\n");

            list.insert(4);
            list.print(self);
            out_string("\n");
        }    
    };
};