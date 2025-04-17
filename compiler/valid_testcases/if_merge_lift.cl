class Main inherits IO {
    main() : Object {
        let x : Object <- 
            if 1 = 1 then
                1
            else
                abort()
            fi
        in

        let y : Int <- 
            case x of
                i : Int => i;
            esac
        in

        out_int(y)  
    };
};