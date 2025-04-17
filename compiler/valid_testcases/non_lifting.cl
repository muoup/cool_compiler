class Main inherits IO {
    main() : Object {
        let x : Int <-
            if 1 = 1 then
                0
            else
                1
            fi,
            y : Int <-
            case 2 of
                i : Int => i;
            esac
        in
        {
            out_int(x);
            out_int(y);
        } 
    };
};