class Main inherits IO {
    main() : Object {
        let x : Int <- in_int() in
        let y : Int <- in_int() in
        {
            out_int(x + y);
            out_int(x - y);
            out_int(x * y);
            out_int(x / y);
            out_int(~x);
            out_int(~y);
        }      
    };
};