class Base {
	procedure(x : Int) : Object { 0 };	
};

class Derived inherits Base {
	procedure(x : Object) : Object { 0 };
};

class Main {
	main() : Object { 0 };
};
