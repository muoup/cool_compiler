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

    elem() : Object {
        if (isvoid head) then
            0
        else
            head
        fi
    };

    next() : IntList {
        tail
    };

    null() : Object {
        let x : Object in x
    };

    remove(i : Int) : Int {
        if (isvoid head) then
            0
        else
            let head_int : Int <- case head of
                i : Int => i;
            esac in

            if (head_int = i) then
                if (isvoid tail) then
                    head <- null()
                else 
                {
                    head <- tail.elem();
                    tail <- tail.next();

                    if (isvoid tail) then
                        tail <- new IntList
                    else
                        0
                    fi;
                } 
                fi
            else
                tail.remove(i)
            fi
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

            list.remove(2);
            list.print(self);
            out_string("\n");

            list.remove(4);
            list.print(self);
            out_string("\n");
        }    
    };
};