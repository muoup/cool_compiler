class Main inherits IO {
    main() : Object {
        let s1 : Object <- "Hello, world!",
            s2 : Object <- "Hello, world!",
            s3 : Object <- "Bye, world!",
            i1 : Object <- 1,
            i2 : Object <- 1,
            i3 : Object <- 2,
            b1 : Object <- true,
            b2 : Object <- true,
            b3 : Object <- false,
            o1 : Object <- new Object,
            o2 : Object <- new Object,
            o3 : Object <- o1,
            v  : Object
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

            if i1 < i3 then
                out_string("i1 < i3\n")
            else
                out_string("i1 >= i3\n")
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

            if b1 < i1 then
                out_string("b1 < i1\n")
            else
                out_string("b1 >= i1\n")
            fi;

            if o1 = o2 then
                out_string("o1 == o2\n")
            else
                out_string("o1 != o2\n")
            fi;

            if o1 = o3 then
                out_string("o1 == o3\n")
            else
                out_string("o1 != o3\n")
            fi;

            if v = o1 then
                out_string("v == o1\n")
            else
                out_string("v != o1\n")
            fi;
        }
    };
};