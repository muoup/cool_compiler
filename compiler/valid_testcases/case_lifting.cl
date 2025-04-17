class Main inherits IO {
    main() : Object {
        let x : Object <-
            case 2 of
                i : Int => i;
                o : Object => abort();
            esac
        in

        let i : Int <- 
            case x of
                i : Int => i;
                o : Object => 0;
            esac
        in

        out_int(i)
    };
};