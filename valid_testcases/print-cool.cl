class Main inherits IO {
	x : Int <- 5;
	self : Int <- 5;

    main() : SELF_TYPE {
	{
	    out_string((new Object).type_name().substr(4,1)).
	    out_string((isvoid self).type_name().substr(1,3));
	    out_string("\n");
	}
    };
};
