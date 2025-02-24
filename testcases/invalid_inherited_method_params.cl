class Basest {
	get_val(x: String) : Int {
		0
	};
};

class Baser inherits Basest{
	get_val(x: String) : Int {
		0
	};
};

class Base inherits Baser{
	get_val(x: String) : Int {
		0
	};
};

class Derived inherits Base{
	get_val(x : Int) : Int {
		1
	};
};

class Main {
	main() : Object { 0 };
};
