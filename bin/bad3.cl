class Base inherits IO{
	a: Int;
	b: Int;

	int_or_object(non_void_arg : Object) : Object {
		if (isvoid non_void_arg) then 
			a
		else
			b
		fi
	};
};

class Main {
	main() : Object {
		let object : Base <- new Base in
		let i : Int <- 0 in
		if o = object.int_or_object(2) then
			1
		else
			0
		fi
	};
};
