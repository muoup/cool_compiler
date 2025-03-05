class Base inherits IO{
	a: Int;
	b: String;

	somethingrandom(k : Object) : Object {
		if (isvoid k) then 
			a
		else
			b
		fi
	};
};

class Main {
	main() : Object {
		let object : Base <- new Base in
		let int : Int <- 0 in
		if int = object then
			1
		else
			0
		fi
	};
};
