-- class B {
--     s : Int <- 5;
-- 	f(): Int { 1 };
-- };


class Main inherits IO{
    str : String <- "3s3";
	main() : Object { 
    case str of
	    a : Int => out_int(a);
        c : Int => out_int(c);
	    b : String => out_string(b);
    esac
    };
};