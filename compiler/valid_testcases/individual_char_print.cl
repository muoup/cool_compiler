class Main inherits IO {
    main() : Object {
        let str : String <- "Hello, world!" in
        let buf : String <- "" in
        let i : Int <- 0 in
        {
            while i < str.length() loop
            {
                out_string(str.substr(i, 1));
                buf <- buf.concat(str.substr(i, 1));
                i <- i + 1;
            }
            pool;
            out_string("\n");
            out_string(buf);
        }
    };
};