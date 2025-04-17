class Main inherits IO {
    main() : Object {
        let s1 : String <- "Hello, world!",
            s2 : String <- "Hello, world!",
            s3 : String <- "Bye, world!",
            i1 : Int <- 1,
            i2 : Int <- 1,
            i3 : Int <- 2,
            b1 : Bool <- true,
            b2 : Bool <- true,
            b3 : Bool <- false
        in
        {
            if s1 = s2 then
                out_string("s1 == s2\n")
            else
                out_string("s1 != s2\n")
            fi;

            if s1 < s3 then
                out_string("s1 < s3\n")
            else
                out_string("s1 >= s3\n")
            fi;

            if i1 = i2 then
                out_string("i1 == i2\n")
            else
                out_string("i1 != i2\n")
            fi;

            if not (i1 < i3) then
                out_string("i1 >= i3\n")
            else
                out_string("i1 < i3\n")
            fi;

            if b1 = b2 then
                out_string("b1 == b2\n")
            else
                out_string("b1 != b2\n")
            fi;

            if b1 < b3 then
                out_string("b1 < b3\n")
            else
                out_string("b1 >= b3\n")
            fi;
        }
    };
};