class Main inherits IO {
	main() : Object {
        let i : Int in
        {
            while i <= 10000 loop
                i <- i + 1
            pool;
            out_int(i);
        }
	};
};
