class Main inherits IO {
    main() : Object {
        let t : Bool <- true in
        let f : Bool <- false in
        {
            if t = f then
                out_string("True == False")
            else
                out_string("True != False")
            fi;

            if t = t then
                out_string("True == True")
            else
                out_string("True != True")
            fi;

            if f = f then
                out_string("False == False")
            else
                out_string("False != False")
            fi; 
        }
    };
};